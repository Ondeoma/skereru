package skereru.web

import io.circe.syntax.*
import org.scalajs.dom
import skereru.shared.*

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.typedarray.{DataView, Uint8Array}

/**
 * メールのファイル（mbox・.eml）の取り込み.
 * 大きな mbox（Google Takeout は数 GB になる）でも、ブラウザで少しずつ読んで1通ずつに分け、数 MB ずつサーバーへ送る
 * （1回の要求が短いので、ロードバランサーのタイムアウトにかからない. 途中で止まっても、やり直せば重複は取り込まれない）.
 */
object FileImport {

  /** 進み具合: 読んだ量・全体・取り込んだ数・重複・失敗・読んでいるファイル. */
  final case class Progress(read: Double, total: Double, imported: Int, skipped: Int, failed: Int, file: String)

  private val Chunk      = 4 * 1024 * 1024  // ファイルを読む単位
  private val Batch      = 6 * 1024 * 1024  // 1回に送る量の目安
  private val MaxMessage = 24 * 1024 * 1024 // サーバーが1回に受け取れる量に収まる1通の大きさ

  /** b の i から "From " が始まるか. */
  private def startsFrom(b: Uint8Array, i: Int): Boolean =
    i + 4 < b.length && b(i) == 0x46 && b(i + 1) == 0x72 && b(i + 2) == 0x6f && b(i + 3) == 0x6d && b(i + 4) == 0x20

  /** mbox の "From " 行（区切り）の位置. 行頭の "From " だけを区切りとみなす. */
  private def isFromLine(b: Uint8Array, i: Int): Boolean = startsFrom(b, i) && (i == 0 || b(i - 1) == 0x0a)

  /** 1通（"From " 行を含む）から、区切りの行を除き、">From " のエスケープ（mboxrd）を戻す. */
  private def unmbox(b: Uint8Array): Uint8Array = {
    var start = 0
    if (isFromLine(b, 0)) { while (start < b.length && b(start) != 0x0a) start += 1; start += 1 }
    val out = new Uint8Array(b.length - math.min(start, b.length))
    var o = 0
    var lineStart = true
    var i = start
    while (i < b.length) {
      if (lineStart && b(i) == 0x3e) {
        // 行頭の ">…>From " は、> を1つ取る
        var j = i
        while (j < b.length && b(j) == 0x3e) j += 1
        if (startsFrom(b, j)) i += 1
      }
      out(o) = b(i)
      o += 1
      lineStart = b(i) == 0x0a
      i += 1
    }
    out.subarray(0, o)
  }

  private def read(file: dom.File, from: Double, to: Double): Future[Uint8Array] =
    file.slice(from, to).arrayBuffer().toFuture.map(new Uint8Array(_))

  /** .eml（1通）か mbox（何通か）か. 先頭が "From " なら mbox. */
  private def isMbox(file: dom.File): Future[Boolean] = read(file, 0, 5).map(isFromLine(_, 0))

  private def each[A](xs: List[A])(f: A => Future[Unit]): Future[Unit] = xs match {
    case Nil    => Future.unit
    case h :: t => f(h).flatMap(_ => each(t)(f))
  }

  private def whileF(cond: => Boolean)(body: => Future[Unit]): Future[Unit] =
    if (!cond) Future.unit else body.flatMap(_ => whileF(cond)(body))

  /**
   * 選んだファイル（mbox・.eml. いくつでも）を、1つの取り込みとして取り込む. onProgress で進み具合を知らせる.
   * cancelled() が true になったら止める（サーバーの取り込みも取り消す）.
   * ラベルのない（Gmail 以外の）メールは、mbox ならファイル名のフォルダ、.eml なら「取り込み」に入れる.
   */
  def run(fileList: dom.FileList, onProgress: Progress => Unit, cancelled: () => Boolean): Future[Progress] = {
    val files = (0 until fileList.length).map(fileList(_)).toList
    var counts = Progress(0, files.map(_.size).sum, 0, 0, 0, "")
    if (files.isEmpty) return Future.successful(counts)
    val name = if (files.size == 1) files.head.name else s"${files.head.name} ほか ${files.size - 1} 件"
    def report(): Unit = onProgress(counts)

    Api.post[ImportJobDto]("/api/imports/file", FileImportReq(name).asJson).flatMap { job =>
      val frames = ListBuffer.empty[Uint8Array]
      var frameBytes = 0
      var tooLarge = 0
      var folder = "取り込み"

      // [4バイトの長さ][メッセージ] の並びにして送る
      def flush(): Future[Unit] =
        if (frames.isEmpty && tooLarge == 0) Future.unit
        else {
          val body = new Uint8Array(frameBytes)
          val view = new DataView(body.buffer)
          var o = 0
          frames.foreach { f =>
            view.setUint32(o, f.length.toDouble)
            body.set(f, o + 4)
            o += 4 + f.length
          }
          val url = s"/api/imports/${job.id}/messages?folder=${Api.enc(folder)}&tooLarge=$tooLarge"
          Api.postBytes[ImportBatchDto](url, body).map { r =>
            counts = counts.copy(imported = counts.imported + r.imported, skipped = counts.skipped + r.skipped, failed = counts.failed + r.failed)
            frames.clear(); frameBytes = 0; tooLarge = 0
            report()
          }
        }

      def add(msg: Uint8Array): Future[Unit] =
        if (msg.length == 0) Future.unit
        else if (msg.length > MaxMessage) { tooLarge += 1; Future.unit }
        else {
          frames += msg
          frameBytes += 4 + msg.length
          if (frameBytes >= Batch) flush() else Future.unit
        }

      // mbox: 読んだ分の中で "From " の行ごとに分ける. 最後の1通は、次の読み込みとつなげる.
      // フォルダが変わるので、前のファイルの分は先に送る
      def mbox(file: dom.File, done: Double): Future[Unit] = flush().flatMap { _ =>
        folder = s"取り込み/${file.name.replaceAll("(?i)\\.(mbox|mbx)$", "")}"
        var carry = new Uint8Array(0)
        var pos = 0.0
        whileF(pos < file.size && !cancelled()) {
          read(file, pos, pos + Chunk).flatMap { chunk =>
            val buf = new Uint8Array(carry.length + chunk.length)
            buf.set(carry)
            buf.set(chunk, carry.length)
            val msgs = ListBuffer.empty[Uint8Array]
            var start = 0
            var i = 1
            while (i < buf.length) {
              if (buf(i - 1) == 0x0a && isFromLine(buf, i)) { msgs += unmbox(buf.subarray(start, i)); start = i }
              i += 1
            }
            each(msgs.toList)(add).map { _ =>
              carry = new Uint8Array(buf.subarray(start, buf.length))
              // 区切りが見つからないまま大きくなりすぎた1通は、読み捨てる
              if (carry.length > MaxMessage * 2) { tooLarge += 1; carry = new Uint8Array(0) }
              counts = counts.copy(read = done + math.min(file.size, pos + Chunk))
              report()
              pos += Chunk
            }
          }
        }.flatMap(_ => if (!cancelled()) add(unmbox(carry)) else Future.unit).flatMap(_ => flush())
      }

      var done = 0.0
      val work = each(files) { file =>
        if (cancelled()) Future.unit
        else {
          counts = counts.copy(file = file.name)
          isMbox(file).flatMap { m =>
            if (m) mbox(file, done)
            else { folder = "取り込み"; read(file, 0, file.size).flatMap(add) } // .eml（1通）
          }.map { _ =>
            done += file.size
            counts = counts.copy(read = done)
            report()
          }
        }
      }
      work.flatMap { _ =>
        if (cancelled()) Api.postUnit(s"/api/imports/${job.id}/cancel")
        else flush().flatMap(_ => Api.postUnit(s"/api/imports/${job.id}/finish"))
      }.map { _ => report(); counts }
        .recoverWith { case e =>
          // 失敗したら取り消して、次の取り込みを始められるようにする
          Api.postUnit(s"/api/imports/${job.id}/cancel").recover { case _ => () }.flatMap(_ => Future.failed(e))
        }
    }
  }

}

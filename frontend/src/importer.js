// メールのファイル（mbox・.eml）の取り込み。Scala.js からは window.skereruImport.importFile(file, opts) で使う。
// 大きな mbox（Google Takeout は数 GB になる）でも、ブラウザで少しずつ読んで1通ずつに分け、数 MB ずつサーバーへ送る
// （1回の要求が短いので、ロードバランサーのタイムアウトにかからない。途中で止まっても、やり直せば重複は取り込まれない）。

const CHUNK = 4 * 1024 * 1024        // ファイルを読む単位
const BATCH = 6 * 1024 * 1024        // 1回に送る量の目安
const MAX_MESSAGE = 24 * 1024 * 1024 // サーバーが1回に受け取れる量に収まる1通の大きさ

const HEADERS = { 'X-Skereru': '1' }

async function call(method, url, body, type) {
  const headers = { ...HEADERS }
  if (type) headers['Content-Type'] = type
  const res = await fetch(url, { method, headers, body, credentials: 'same-origin' })
  if (!res.ok) {
    let msg = `${res.status}`
    try { msg = (await res.json()).error || msg } catch (_) { /* 本文が JSON でない */ }
    throw new Error(msg)
  }
  return res.json()
}

/** mbox の "From " 行（区切り）の位置。行頭の "From " だけを区切りとみなす。 */
function isFromLine(b, i) {
  return b[i] === 0x46 && b[i + 1] === 0x72 && b[i + 2] === 0x6f && b[i + 3] === 0x6d && b[i + 4] === 0x20 && (i === 0 || b[i - 1] === 0x0a)
}

/** 1通（"From " 行を含む）から、区切りの行を除き、">From " のエスケープ（mboxrd）を戻す。 */
function unmbox(b) {
  let start = 0
  if (isFromLine(b, 0)) { while (start < b.length && b[start] !== 0x0a) start++; start++ }
  const out = new Uint8Array(b.length - Math.min(start, b.length))
  let o = 0
  let lineStart = true
  for (let i = start; i < b.length; i++) {
    if (lineStart && b[i] === 0x3e) {
      // 行頭の ">…>From " は、> を1つ取る
      let j = i
      while (j < b.length && b[j] === 0x3e) j++
      if (b[j] === 0x46 && b[j + 1] === 0x72 && b[j + 2] === 0x6f && b[j + 3] === 0x6d && b[j + 4] === 0x20) { i++ }
    }
    out[o++] = b[i]
    lineStart = b[i] === 0x0a
  }
  return out.subarray(0, o)
}

/** .eml（1通）か mbox（何通か）か。先頭が "From " なら mbox。 */
async function isMbox(file) {
  const head = new Uint8Array(await file.slice(0, 5).arrayBuffer())
  return isFromLine(head, 0)
}

/**
 * 選んだファイル（mbox・.eml。いくつでも）を、1つの取り込みとして取り込む。
 * opts.onProgress({ read, total, imported, skipped, failed, file }) で進み具合を知らせる。
 * opts.isCancelled() が true になったら止める（サーバーの取り込みも取り消す）。
 * ラベルのない（Gmail 以外の）メールは、mbox ならファイル名のフォルダ、.eml なら「取り込み」に入れる。
 */
export async function importFiles(fileList, opts = {}) {
  const files = Array.from(fileList)
  if (files.length === 0) return null
  const name = files.length === 1 ? files[0].name : `${files[0].name} ほか ${files.length - 1} 件`
  const job = await call('POST', '/api/imports/file', JSON.stringify({ name }), 'application/json')
  const counts = { read: 0, total: files.reduce((n, f) => n + f.size, 0), imported: 0, skipped: 0, failed: 0, file: '' }
  const report = () => opts.onProgress && opts.onProgress({ ...counts })
  const cancelled = () => !!(opts.isCancelled && opts.isCancelled())

  let frames = []
  let frameBytes = 0
  let tooLarge = 0
  let folder = '取り込み'
  const flush = async () => {
    if (frames.length === 0 && tooLarge === 0) return
    const body = new Uint8Array(frameBytes)
    const view = new DataView(body.buffer)
    let o = 0
    for (const f of frames) {
      view.setUint32(o, f.length)
      body.set(f, o + 4)
      o += 4 + f.length
    }
    const url = `/api/imports/${job.id}/messages?folder=${encodeURIComponent(folder)}&tooLarge=${tooLarge}`
    const r = await call('POST', url, body, 'application/octet-stream')
    counts.imported += r.imported; counts.skipped += r.skipped; counts.failed += r.failed
    frames = []; frameBytes = 0; tooLarge = 0
    report()
  }
  const add = async (msg) => {
    if (msg.length === 0) return
    if (msg.length > MAX_MESSAGE) { tooLarge++; return }
    frames.push(msg); frameBytes += 4 + msg.length
    if (frameBytes >= BATCH) await flush()
  }

  try {
    let done = 0
    for (const file of files) {
      if (cancelled()) break
      counts.file = file.name
      if (!(await isMbox(file))) {
        // .eml（1通）
        folder = '取り込み'
        await add(new Uint8Array(await file.arrayBuffer()))
      } else {
        // mbox: 読んだ分の中で "From " の行ごとに分ける。最後の1通は、次の読み込みとつなげる。
        // フォルダが変わるので、前のファイルの分は先に送る
        await flush()
        folder = `取り込み/${file.name.replace(/\.(mbox|mbx)$/i, '')}`
        let carry = new Uint8Array(0)
        for (let pos = 0; pos < file.size && !cancelled(); pos += CHUNK) {
          const chunk = new Uint8Array(await file.slice(pos, pos + CHUNK).arrayBuffer())
          const buf = new Uint8Array(carry.length + chunk.length)
          buf.set(carry); buf.set(chunk, carry.length)
          let start = 0
          for (let i = 1; i < buf.length; i++) {
            if (buf[i - 1] === 0x0a && isFromLine(buf, i)) {
              await add(unmbox(buf.subarray(start, i)))
              start = i
            }
          }
          carry = buf.slice(start)
          // 区切りが見つからないまま大きくなりすぎた1通は、読み捨てる
          if (carry.length > MAX_MESSAGE * 2) { tooLarge++; carry = new Uint8Array(0) }
          counts.read = done + Math.min(file.size, pos + CHUNK)
          report()
        }
        if (!cancelled()) await add(unmbox(carry))
        await flush()
      }
      done += file.size
      counts.read = done
      report()
    }
    if (cancelled()) {
      await call('POST', `/api/imports/${job.id}/cancel`)
    } else {
      await flush()
      await call('POST', `/api/imports/${job.id}/finish`)
    }
    report()
    return { ...counts }
  } catch (e) {
    // 失敗したら取り消して、次の取り込みを始められるようにする
    try { await call('POST', `/api/imports/${job.id}/cancel`) } catch (_) { /* すでに終わっている */ }
    throw e
  }
}

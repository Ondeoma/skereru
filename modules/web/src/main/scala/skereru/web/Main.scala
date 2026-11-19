package skereru.web

import io.github.ondeoma.scalactive.ScalactiveConfig
import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom

/**
 * 画面の入口.
 * `@main def main()` にすると生成される `main` クラスと `Main` が、大文字小文字を区別しないファイルシステム（macOS）で
 * 同じファイル名になり差分コンパイルが壊れるため、通常の main(args) にする.
 */
object Main {

  def main(args: Array[String]): Unit = {
    ScalactiveConfig.titleTemplate = "%s | skereru"
    ScalactiveConfig.defaultTitle = Some("skereru")
    AppNames.init("skereru")
    Theme.apply()
    // アイコンのボタンなどのヒント（title をすぐに出す）
    Tooltip.install()
    // CSRF の対策: 共通部品（Pwa・Observe）の要求にも X-Skereru を付ける（Api も同じヘッダーを使う）
    sbirds.web.Fetch.extraHeaders = Map("X-Skereru" -> "1")
    // skorimako のブラウザの観測（設定があるときだけ）と、PWA（ホーム画面に追加・オフラインで開く・プッシュ通知）
    Observe.start()
    Pwa.register()
    App()(dom.document.body, AddMethod.appendS(IdSelector("app"))).init()
  }

}

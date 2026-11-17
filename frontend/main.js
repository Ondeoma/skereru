import './style.css'
import { createEditor } from './src/editor.js'
import { installTooltips } from './src/tooltip.js'
import { importFiles } from './src/importer.js'

// Scala.js の画面から使うエディタ（Tiptap）
window.skereruEditor = { create: createEditor }
// メールのファイル（mbox・.eml）の取り込み
window.skereruImport = { importFiles }

// アイコンのボタンなどのヒント（title をすぐに出す）
installTooltips()

import 'scalajs:main.js'

import './style.css'
import { createEditor } from './src/editor.js'

// Scala.js の画面から使うエディタ（Tiptap. ライブラリを呼ぶ所だけ JS）
window.skereruEditor = { create: createEditor }

import 'scalajs:main.js'

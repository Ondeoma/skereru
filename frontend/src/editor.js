// メール作成のリッチテキストエディタ（Tiptap）。ライブラリを呼ぶ所だけをここに置く。
// ツールバー・ファイルのドロップ・リンクの入力などの画面の処理は Scala.js（modules/web の Editor）。
// 出力は HTML（送信時にサーバーで無害化する）とテキスト（text/plain の本文）。
import { Editor } from '@tiptap/core'
import StarterKit from '@tiptap/starter-kit'
import { Placeholder } from '@tiptap/extensions'

// ツールバーの操作（名前 → Tiptap のコマンド）
const COMMANDS = {
  bold: (c) => c.toggleBold(),
  italic: (c) => c.toggleItalic(),
  underline: (c) => c.toggleUnderline(),
  strike: (c) => c.toggleStrike(),
  bulletList: (c) => c.toggleBulletList(),
  orderedList: (c) => c.toggleOrderedList(),
  blockquote: (c) => c.toggleBlockquote(),
  setLink: (c, href) => c.extendMarkRange('link').setLink({ href }),
  unsetLink: (c) => c.extendMarkRange('link').unsetLink(),
  clearFormat: (c) => c.unsetAllMarks().clearNodes(),
  // リンクを入れる（arg は {"href", "text"} の JSON）。後ろに空白を足して、続けて打つ文字がリンクにならないようにする
  insertLink: (c, arg) => {
    const { href, text } = JSON.parse(arg)
    return c.insertContent([{ type: 'text', text, marks: [{ type: 'link', attrs: { href } }] }, { type: 'text', text: ' ' }])
  },
}

/** el に Tiptap のエディタを作る。opts: html・placeholder・onUpdate・onSelectionUpdate。 */
export function createEditor(el, opts) {
  const editor = new Editor({
    element: el,
    extensions: [
      StarterKit.configure({
        heading: false, codeBlock: false, horizontalRule: false,
        link: { openOnClick: false, autolink: true, protocols: ['http', 'https', 'mailto'], HTMLAttributes: { rel: 'noopener noreferrer', target: '_blank' } },
      }),
      Placeholder.configure({ placeholder: opts.placeholder || '' }),
    ],
    content: opts.html || '',
    onUpdate: () => opts.onUpdate && opts.onUpdate(),
    onSelectionUpdate: () => opts.onSelectionUpdate && opts.onSelectionUpdate(),
  })
  return {
    run: (name, arg) => { COMMANDS[name](editor.chain().focus(), arg).run() },
    isActive: (name) => editor.isActive(name),
    linkHref: () => editor.getAttributes('link').href || '',
    getHTML: () => editor.getHTML(),
    // テキストの本文（段落・改行・引用・箇条書きを保つ）
    getText: () => editor.getText({ blockSeparator: '\n' }),
    isEmpty: () => editor.isEmpty,
    focus: () => { editor.commands.focus('start') },
    destroy: () => { editor.destroy() },
  }
}

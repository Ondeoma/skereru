// メール作成のリッチテキストエディタ（Tiptap）。Scala.js からは window.skereruEditor.create(el, opts) で使う。
// 出力は HTML（送信時にサーバーで無害化する）とテキスト（text/plain の本文）。
import { Editor } from '@tiptap/core'
import StarterKit from '@tiptap/starter-kit'
import { Placeholder } from '@tiptap/extensions'

// 確認や入力には、ブラウザ標準のダイアログ（window.prompt など）を使わない。アプリのダイアログ（opts.prompt）を使う
const BUTTONS = [
  ['B', '太字', (e) => e.chain().focus().toggleBold().run(), (e) => e.isActive('bold')],
  ['I', '斜体', (e) => e.chain().focus().toggleItalic().run(), (e) => e.isActive('italic')],
  ['U', '下線', (e) => e.chain().focus().toggleUnderline().run(), (e) => e.isActive('underline')],
  ['S', '取り消し線', (e) => e.chain().focus().toggleStrike().run(), (e) => e.isActive('strike')],
  ['•', '箇条書き', (e) => e.chain().focus().toggleBulletList().run(), (e) => e.isActive('bulletList')],
  ['1.', '番号付き', (e) => e.chain().focus().toggleOrderedList().run(), (e) => e.isActive('orderedList')],
  ['❝', '引用', (e) => e.chain().focus().toggleBlockquote().run(), (e) => e.isActive('blockquote')],
  ['🔗', 'リンク', (e, opts) => {
    const prev = e.getAttributes('link').href || ''
    if (!opts.prompt) return
    // 取り消したときは呼ばれない。ダイアログの間にフォーカスが外れても、選択範囲はエディタが覚えている
    opts.prompt('リンクの URL（空にすると外す。http・https・mailto）', prev, (url) => {
      if (url.trim() === '') e.chain().focus().extendMarkRange('link').unsetLink().run()
      else if (/^(https?:|mailto:)/i.test(url.trim())) e.chain().focus().extendMarkRange('link').setLink({ href: url.trim() }).run()
    })
  }, (e) => e.isActive('link')],
  ['⌫', '書式を消す', (e) => e.chain().focus().unsetAllMarks().clearNodes().run(), () => false],
]

/** テキストの本文（段落・改行・引用・箇条書きを保つ）。 */
function toText(editor) {
  return editor.getText({ blockSeparator: '\n' }).replace(/ /g, ' ')
}

export function createEditor(el, opts) {
  const wrap = document.createElement('div')
  wrap.className = 'mail-editor'
  const bar = document.createElement('div')
  bar.className = 'mail-editor-bar'
  const area = document.createElement('div')
  area.className = 'mail-editor-area'
  wrap.append(bar, area)
  el.append(wrap)

  const editor = new Editor({
    element: area,
    extensions: [
      StarterKit.configure({
        heading: false, codeBlock: false, horizontalRule: false,
        link: { openOnClick: false, autolink: true, protocols: ['http', 'https', 'mailto'], HTMLAttributes: { rel: 'noopener noreferrer', target: '_blank' } },
      }),
      Placeholder.configure({ placeholder: opts.placeholder || '' }),
    ],
    content: opts.html || '',
    onUpdate: () => { opts.onChange && opts.onChange(); refresh() },
    onSelectionUpdate: () => refresh(),
  })

  const buttons = BUTTONS.map(([label, title, run, active]) => {
    const b = document.createElement('button')
    b.type = 'button'
    b.textContent = label
    b.title = title
    b.addEventListener('mousedown', (ev) => ev.preventDefault())
    b.addEventListener('click', () => run(editor, opts))
    bar.append(b)
    return [b, active]
  })
  function refresh() { buttons.forEach(([b, active]) => b.classList.toggle('on', !!active(editor))) }

  // ファイルのドロップで添付する（opts.onFiles に渡す）。本文に画像などとして入れたり、ブラウザがファイルを開いて
  // 書きかけが消えたりしないよう、エディタより先に（capture で）受け取る。文字のドラッグ（並べ替え）はそのまま通す
  const hasFiles = (ev) => !!ev.dataTransfer && Array.from(ev.dataTransfer.types || []).includes('Files')
  let depth = 0
  wrap.addEventListener('dragenter', (ev) => { if (hasFiles(ev)) { depth++; wrap.classList.add('dropping') } }, true)
  wrap.addEventListener('dragleave', (ev) => { if (hasFiles(ev) && --depth <= 0) { depth = 0; wrap.classList.remove('dropping') } }, true)
  wrap.addEventListener('dragover', (ev) => {
    if (!hasFiles(ev)) return
    ev.preventDefault()
    ev.dataTransfer.dropEffect = 'copy'
  }, true)
  wrap.addEventListener('drop', (ev) => {
    if (!hasFiles(ev)) return
    ev.preventDefault()
    ev.stopPropagation()
    depth = 0
    wrap.classList.remove('dropping')
    const files = Array.from(ev.dataTransfer.files || [])
    if (files.length && opts.onFiles) opts.onFiles(files)
  }, true)

  return {
    getHTML: () => editor.getHTML(),
    getText: () => toText(editor),
    isEmpty: () => editor.isEmpty,
    focus: () => editor.commands.focus('start'),
    destroy: () => { editor.destroy(); wrap.remove() },
  }
}

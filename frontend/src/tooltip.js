// ツールチップ: title の付いた要素にカーソルを重ねる（またはキーボードでフォーカスする）と、すぐにヒントを出す。
// ブラウザ標準の title のツールチップは出るまでに時間がかかり気付きにくいので、表示中は title を data-tip に移して標準のものを止める。
// 画面（Scala.js）側は今までどおり title を書くだけでよい。後から足された要素にも効く（イベントの委譲）。

const DELAY_MS = 250

let tip = null
let current = null
let timer = 0

function el() {
  if (!tip) {
    tip = document.createElement('div')
    tip.className = 'tooltip'
    tip.setAttribute('role', 'tooltip')
    document.body.append(tip)
  }
  return tip
}

function target(node) {
  return node instanceof Element ? node.closest('[title], [data-tip]') : null
}

function show(t) {
  const text = t.getAttribute('data-tip') || ''
  if (!text) return
  const box = el()
  box.textContent = text
  box.classList.add('on')
  const r = t.getBoundingClientRect()
  const w = box.offsetWidth
  const h = box.offsetHeight
  // ふだんは下に出す。画面の下に収まらなければ上に出す。左右は画面からはみ出さないようにする
  let top = r.bottom + 6
  if (top + h > window.innerHeight - 4) top = r.top - h - 6
  const left = Math.min(Math.max(4, r.left + r.width / 2 - w / 2), window.innerWidth - w - 4)
  box.style.top = `${Math.round(top)}px`
  box.style.left = `${Math.round(left)}px`
}

function enter(t) {
  if (current === t) return
  leave()
  current = t
  // 標準のツールチップを止める（data-tip に移す）
  if (t.hasAttribute('title')) {
    t.setAttribute('data-tip', t.getAttribute('title'))
    t.removeAttribute('title')
  }
  if (!t.hasAttribute('aria-label') && !t.textContent.trim()) t.setAttribute('aria-label', t.getAttribute('data-tip'))
  timer = window.setTimeout(() => current === t && show(t), DELAY_MS)
}

function leave() {
  window.clearTimeout(timer)
  if (current) {
    // 画面側が title を書き換えることがあるので、戻しておく
    if (!current.hasAttribute('title') && current.hasAttribute('data-tip')) current.setAttribute('title', current.getAttribute('data-tip'))
    current = null
  }
  if (tip) tip.classList.remove('on')
}

export function installTooltips() {
  // タッチだけの端末ではカーソルを重ねられないので出さない（長押しのメニューと重ならないように）
  const hover = window.matchMedia('(hover: hover)')
  document.addEventListener('mouseover', (ev) => {
    if (!hover.matches) return
    const t = target(ev.target)
    if (t) enter(t)
    else if (current) leave()
  })
  document.addEventListener('focusin', (ev) => {
    const t = target(ev.target)
    if (t && t.matches(':focus-visible')) enter(t)
  })
  document.addEventListener('focusout', () => leave())
  // カーソルが画面の外に出たとき
  document.addEventListener('mouseout', (ev) => { if (!ev.relatedTarget) leave() })
  // 押したら消す（メニューが開く・画面が変わるなど）
  document.addEventListener('mousedown', () => leave(), true)
  document.addEventListener('keydown', (ev) => { if (ev.key === 'Escape') leave() })
  window.addEventListener('scroll', () => leave(), true)
}

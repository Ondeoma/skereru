// 画面の色（手動の指定）を最初の描画の前に反映する（CSP でインラインのスクリプトを禁止しているため別ファイル）
try { var t = localStorage.getItem('skereru.theme'); if (t === 'light' || t === 'dark') document.documentElement.setAttribute('data-theme', t) } catch (e) {}

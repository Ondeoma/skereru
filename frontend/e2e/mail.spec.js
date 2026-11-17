import { test, expect } from '@playwright/test'
import { H, login, purgeBySubject, unique } from './helpers.js'

test.beforeEach(async ({ page }) => {
  await login(page)
})

test('受信トレイ: 一覧・未読の数・ヘッダーのアドレス', async ({ page }) => {
  await page.goto('/mail/inbox')
  await expect(page.locator('.list .row').first()).toBeVisible()
  await expect(page.locator('.me-addr')).toHaveText('yamada@demo.example')
  await expect(page).toHaveTitle(/受信トレイ \| skereru/)
})

test('作成・送信の取り消し・送信', async ({ page }) => {
  const subject = unique('E2E 送信')
  await page.goto('/mail/inbox')
  await page.getByRole('button', { name: /作成/ }).click()
  await page.locator('.line.rel input[type=text]').first().fill('yamada@demo.example')
  await page.getByPlaceholder('件名').fill(subject)
  await page.locator('.mail-editor .tiptap').click()
  await page.keyboard.type('E2E の本文です')
  // 送信 → 取り消す → 作成画面に戻る
  await page.getByRole('button', { name: '送信', exact: true }).click()
  await page.getByRole('button', { name: '取り消す' }).click()
  await expect(page.getByPlaceholder('件名')).toHaveValue(subject)
  // もう一度送り、取り消しの猶予が過ぎたら受信トレイに届く
  await page.getByRole('button', { name: '送信', exact: true }).click()
  await expect(page.locator('.list .row', { hasText: subject }).first()).toBeVisible({ timeout: 30_000 })
  await purgeBySubject(page, subject)
})

test('検索・スター付き・スヌーズ', async ({ page }) => {
  await page.goto('/mail/inbox')
  await page.getByPlaceholder(/メールを検索/).fill('請求書')
  await page.keyboard.press('Enter')
  await expect(page.locator('.list .row', { hasText: '請求書' }).first()).toBeVisible()
  await page.goto('/starred')
  await expect(page).toHaveTitle(/スター付き/)
  // スヌーズの API（5 秒後に戻る）
  const inbox = (await (await page.request.get('/api/mailboxes')).json()).find((b) => b.specialUse === 'inbox')
  const first = (await (await page.request.get(`/api/messages?mailboxId=${inbox.id}&limit=1`)).json()).items[0]
  const until = new Date(Date.now() + 5000).toISOString()
  await page.request.post('/api/messages/snooze', { headers: H, data: { ids: [first.id], until } })
  await page.goto('/snoozed')
  await expect(page.locator('.list .row', { hasText: first.subject }).first()).toBeVisible()
  await expect.poll(async () => (await (await page.request.get('/api/messages?q=is:snoozed')).json()).total, { timeout: 20_000 }).toBe(0)
})

test('キーボードのショートカット', async ({ page }) => {
  await page.goto('/mail/inbox')
  await expect(page.locator('.list .row').first()).toBeVisible()
  await page.keyboard.press('j')
  await expect(page.locator('.list .row.cursor')).toHaveCount(1)
  await page.keyboard.press('Shift+?')
  await expect(page.locator('.kbd-help')).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(page.locator('.kbd-help')).toHaveCount(0)
  await page.keyboard.press('c')
  await expect(page.locator('.mail-editor')).toBeVisible()
})

test('ツールチップ（アイコンにすぐヒントが出る）', async ({ page }) => {
  await page.goto('/mail/inbox')
  await page.locator('button[title="最新の状態に更新"]').hover()
  await expect(page.locator('.tooltip.on')).toHaveText('最新の状態に更新')
})

test('連絡先: 追加・検索・削除', async ({ page }) => {
  const addr = `e2e-${Date.now().toString(36)}@example.org`
  await page.goto('/contacts')
  await page.getByRole('button', { name: '＋ 連絡先を追加' }).click()
  await page.locator('input[type=email]').fill(addr)
  await page.getByPlaceholder('山田 太郎').fill('E2E 連絡先')
  await page.getByRole('button', { name: '保存', exact: true }).click()
  await expect(page.locator('.contact', { hasText: addr })).toBeVisible()
  await page.locator('.contact', { hasText: addr }).locator('button[title^="連絡先から削除"]').click()
  await page.getByRole('button', { name: '削除', exact: true }).click()
  await expect(page.locator('.contact', { hasText: addr })).toHaveCount(0)
})

test('取り込み（mbox）と書き出し', async ({ page }) => {
  const subject = unique('E2E 取り込み')
  const file = `e2e-${Date.now().toString(36)}.mbox`
  const mbox = `From x@example.org Mon Nov 16 10:00:00 2026\nX-Gmail-Labels: Inbox,Starred\nFrom: x@example.org\nTo: yamada@demo.example\nSubject: ${subject}\nMessage-ID: <${Date.now()}@e2e>\nDate: Mon, 16 Nov 2026 10:00:00 +0900\n\nhello\n`
  await page.goto('/settings')
  await page.getByRole('button', { name: '取り込み・書き出し' }).click()
  await page.locator('.dropzone input[type=file]').setInputFiles({ name: file, mimeType: 'application/mbox', buffer: Buffer.from(mbox) })
  // 今回のファイルの行が「完了」になるまで待つ（前の取り込みの行と取り違えない）
  await expect.poll(async () => (await (await page.request.get('/api/imports')).json()).find((j) => j.source === file && j.status === 'done' && j.imported + j.skipped > 0) !== undefined,
    { timeout: 20_000 }).toBe(true)
  const found = await (await page.request.get(`/api/messages?q=${encodeURIComponent(`subject:"${subject}"`)}`)).json()
  expect(found.total).toBe(1)
  expect(found.items[0].flagged).toBeTruthy()
  // 書き出し（受信トレイ）に取り込んだメールが入っている
  const inbox = (await (await page.request.get('/api/mailboxes')).json()).find((b) => b.specialUse === 'inbox')
  const exp = await page.request.get(`/api/export?mailboxId=${inbox.id}`)
  expect(exp.headers()['content-type']).toContain('application/mbox')
  expect(await exp.text()).toContain(subject)
  await purgeBySubject(page, subject)
})

test('管理: DMARC のタブ', async ({ page }) => {
  await page.goto('/admin')
  await page.getByRole('button', { name: 'DMARC' }).click()
  await expect(page.getByText('DMARC の集計レポート')).toBeVisible()
})

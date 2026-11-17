// E2E テストの共通の手順（開発用のログイン・後片付け）
import { expect } from '@playwright/test'

export const H = { 'X-Skereru': '1' }
// 開発用のログインは名前でユーザーを探すので、デモの山田さんと同じ名前にする
export const ME = { name: '山田 太郎', email: 'yamada@demo.example' }

/** 開発用のログイン（DEV_LOGIN=true のサーバーだけ）。Cookie はページと共有される。 */
export async function login(page, who = ME) {
  const r = await page.request.post('/auth/dev-login', { headers: H, data: { name: who.name, email: who.email, admin: true } })
  expect(r.ok()).toBeTruthy()
}

/** 件名で探したメッセージを完全に消す（ゴミ箱へ移してから、ゴミ箱からも消す）。 */
export async function purgeBySubject(page, subject) {
  for (const q of [`subject:"${subject}"`, `subject:"${subject}" in:trash`]) {
    const l = await (await page.request.get(`/api/messages?q=${encodeURIComponent(q)}&limit=100`)).json()
    const ids = (l.items || []).map((m) => m.id)
    if (ids.length) await page.request.post('/api/messages/update', { headers: H, data: { ids, delete: true } })
  }
}

export const unique = (p) => `${p} ${Date.now().toString(36)}`

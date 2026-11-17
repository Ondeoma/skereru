// 画面の E2E テスト（npm run e2e）。開発用のサーバー（バックエンド :8095 と Vite :5179、DEV_LOGIN=true）を先に起動しておく。
// ブラウザはダウンロードせず、この機械の Google Chrome を使う（channel: 'chrome'）。
import { defineConfig } from '@playwright/test'

export default defineConfig({
  testDir: './e2e',
  timeout: 60_000,
  fullyParallel: false,
  workers: 1,
  reporter: [['list']],
  use: {
    baseURL: process.env.E2E_BASE ?? 'http://localhost:5179',
    channel: process.env.E2E_CHANNEL ?? 'chrome',
    headless: true,
    locale: 'ja-JP',
    timezoneId: 'Asia/Tokyo',
    trace: 'retain-on-failure',
  },
})

import { defineConfig } from 'vite'
import scalaJS from './vite-scalajs.js'

const backend = process.env.BACKEND ?? 'http://127.0.0.1:8095'  // DEV_LOGIN のバックエンドは 127.0.0.1 だけで待ち受ける

export default defineConfig({
  // 画面(Scalactive)は sbt の web プロジェクトから読み込む
  plugins: [scalaJS({ cwd: '..', projectID: 'web' })],
  server: {
    // IPv4 で待ち受ける（LAN には公開しない）
    host: '127.0.0.1',
    proxy: { '/api': backend, '/auth': backend, '/mcp': backend, '/.well-known': backend },
  },
  build: { chunkSizeWarningLimit: 2000 },
})

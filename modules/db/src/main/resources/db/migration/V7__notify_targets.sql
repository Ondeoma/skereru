-- stui との OAuth 連携（sbirds.links.IntegrationTokenStore）: ユーザーごとのトークン（APP_SECRET で暗号化）と、認可の途中の state
create table integration_token (
  user_id    uuid not null references app_user(id) on delete cascade,
  provider   text not null,
  token_enc  text not null,
  created_at timestamptz not null default now(),
  primary key (user_id, provider)
);

-- 認可を始めたときの state と PKCE の code_verifier（10分で失効）
create table integration_state (
  state         text primary key,
  user_id       uuid not null references app_user(id) on delete cascade,
  provider      text not null,
  code_verifier text not null,
  return_to     text not null,
  expires_at    timestamptz not null
);

-- 受信の通知先（stui・受信 Webhook・署名付きの Webhook）
create table notify_target (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  kind         text not null,                  -- stui | chat_hook | webhook
  channel_id   text,                           -- stui: 投稿するチャンネル（null なら自分への DM）
  channel_name text,                           -- stui: 表示用のチャンネルの名前
  url          text,                           -- chat_hook・webhook: 送り先
  secret_enc   text,                           -- webhook: 署名の鍵（APP_SECRET で暗号化）
  all_inbox    boolean not null default true,  -- 受信トレイの新着をすべて知らせる
  enabled      boolean not null default true,
  last_error   text,
  created_at   timestamptz not null default now()
);
create index notify_target_user_idx on notify_target (user_id);

-- 通知の送信待ち（SKIP LOCKED で分担し、失敗したら間を空けて送り直す）
create table notify_delivery (
  id          uuid primary key,
  target_id   uuid not null references notify_target(id) on delete cascade,
  payload     jsonb not null,
  not_before  timestamptz not null default now(),
  attempts    int not null default 0,
  status_code int,
  last_error  text,
  done_at     timestamptz,
  failed      boolean not null default false,
  created_at  timestamptz not null default now()
);
create index notify_delivery_pending_idx on notify_delivery (not_before) where done_at is null and not failed;

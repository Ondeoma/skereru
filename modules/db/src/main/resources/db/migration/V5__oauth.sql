-- ------------------------------------------------------------------ OAuth の提供側（skakapo などが利用者の権限で skereru を使う）

-- 認可コード: 60秒で失効、一度だけ使える。ハッシュだけを保存する
create table oauth_code (
  code_hash      bytea primary key,
  client_id      text not null,
  redirect_uri   text not null,
  user_id        uuid not null references app_user(id) on delete cascade,
  code_challenge text not null,
  scope          text not null,
  expires_at     timestamptz not null,
  used_at        timestamptz
);
create index oauth_code_expires_idx on oauth_code (expires_at);

-- 利用者がクライアントに出した許可（連携）。refresh token はハッシュだけを保存し、使うたびに新しくする。
-- 許可し直したら前の許可を消す（その許可で発行したアクセストークンも消える）
create table oauth_grant (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  client_id    text not null,
  scopes       text[] not null,
  refresh_hash bytea not null unique,
  created_at   timestamptz not null default now(),
  refreshed_at timestamptz,
  unique (user_id, client_id)
);

-- 許可で発行したアクセストークン（有効期限つきの API キー）。設定の「APIキー」の一覧には出さず、「連携しているサービス」に出す
alter table api_key add column oauth_grant uuid references oauth_grant(id) on delete cascade;
create index api_key_oauth_grant_idx on api_key (oauth_grant) where oauth_grant is not null;

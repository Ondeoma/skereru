-- skereru のスキーマ。適用済みのファイルは書き換えず、変更は新しい番号のファイルで行う。
create extension if not exists pgroonga;

-- ---------------------------------------------------------------- ユーザー・認証

create table app_user (
  id            uuid primary key,
  oidc_sub      text unique,                -- IdP の sub（管理者が先に作ったユーザーは最初のログインまで null）
  name          text not null,
  email         text,                       -- IdP のメールアドレス（ログイン時の結び付けに使う）
  avatar_url    text,
  is_admin      boolean not null default false,
  disabled      boolean not null default false,
  timezone      text not null default 'Asia/Tokyo',
  quota_bytes   bigint,                     -- null は既定の容量
  used_bytes    bigint not null default 0,  -- メッセージの合計（message のトリガーで更新）
  settings      jsonb not null default '{}'::jsonb,
  created_at    timestamptz not null default now(),
  last_login_at timestamptz
);
create unique index app_user_email_unlinked on app_user (lower(email)) where oidc_sub is null and email is not null;

create table user_group (
  user_id uuid not null references app_user(id) on delete cascade,
  name    text not null,
  primary key (user_id, name)
);

create table session (
  token_hash bytea primary key,
  user_id    uuid not null references app_user(id) on delete cascade,
  created_at timestamptz not null default now(),
  expires_at timestamptz not null,
  ip         text,
  user_agent text
);
create index session_by_user on session (user_id);
create index session_by_expiry on session (expires_at);

create table login_state (
  state      text primary key,
  verifier   text not null,
  nonce      text not null,
  return_to  text not null,
  created_at timestamptz not null default now()
);

-- メールソフト（IMAP・POP3・SMTP）用のアプリパスワード。ハッシュだけを保存する
create table app_password (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  token_hash   bytea not null unique,
  prefix       text not null,
  created_at   timestamptz not null default now(),
  last_used_at timestamptz,
  last_used_ip text
);
create index app_password_user on app_password (user_id);

create table api_key (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  key_hash     bytea not null unique,
  prefix       text not null,
  scopes       text[] not null,
  created_at   timestamptz not null default now(),
  expires_at   timestamptz,
  last_used_at timestamptz
);
create index api_key_user on api_key (user_id);

-- 認証の失敗の回数（総当たり対策。IP・アカウントごと。全ノードで共有する）
create table auth_failure (
  key          text primary key,
  failures     int not null,
  window_start timestamptz not null,
  locked_until timestamptz
);

-- 兄弟のサービスとの連携（skuaka のカレンダーなど）。secret は API トークンを SecretBox で暗号化したもの
create table user_integration (
  user_id    uuid not null references app_user(id) on delete cascade,
  kind       text not null,
  secret     text not null,
  created_at timestamptz not null default now(),
  primary key (user_id, kind)
);

-- ---------------------------------------------------------------- ドメイン・アドレス

create table domain (
  id             uuid primary key,
  name           text not null unique,     -- 小文字・punycode
  enabled        boolean not null default true,
  auto_provision boolean not null default false, -- このドメインのメールアドレスで IdP からログインした人のメールボックスを自動で作る
  catch_all      uuid references app_user(id) on delete set null,
  dkim_selector  text not null,
  dkim_private   text not null,             -- APP_SECRET で暗号化した PKCS#8
  dkim_public    text not null,
  created_at     timestamptz not null default now()
);

-- 配送先のアドレス（ユーザーの主アドレス・別名・転送）
create table address (
  id         uuid primary key,
  address    text not null unique,         -- 小文字
  domain_id  uuid not null references domain(id) on delete cascade,
  user_id    uuid references app_user(id) on delete cascade,
  forward_to text[] not null default '{}',
  is_primary boolean not null default false,
  can_send   boolean not null default true,
  created_at timestamptz not null default now(),
  check (user_id is not null or cardinality(forward_to) > 0)
);
create index address_user on address (user_id);
create unique index address_primary on address (user_id) where is_primary;

-- ---------------------------------------------------------------- メールボックス・メッセージ

create table mailbox (
  id             uuid primary key,
  user_id        uuid not null references app_user(id) on delete cascade,
  name           text not null,
  special_use    text,
  uidvalidity    bigint not null,
  uidnext        bigint not null default 1,
  highest_modseq bigint not null default 1,
  subscribed     boolean not null default true,
  created_at     timestamptz not null default now(),
  unique (user_id, name)
);
create unique index mailbox_special on mailbox (user_id, special_use) where special_use is not null;

-- メッセージの本体（内容の SHA-256 で重複を省く。S3 に置いたときは data が null）
create table blob (
  id         text primary key,
  size       bigint not null,
  data       bytea,
  created_at timestamptz not null default now()
);

create table message (
  id              uuid primary key,
  user_id         uuid not null references app_user(id) on delete cascade,
  mailbox_id      uuid not null references mailbox(id) on delete cascade,
  uid             bigint not null,
  modseq          bigint not null,
  blob_id         text not null references blob(id),
  size            int not null,
  internal_date   timestamptz not null,
  flags           text[] not null default '{}',
  subject         text not null default '',
  from_addr       text,
  from_name       text,
  to_addrs        text not null default '',  -- 表示用（"名前 <addr>, …"）
  cc_addrs        text not null default '',
  sent_at         timestamptz,
  message_id      text,
  in_reply_to     text,
  thread_id       uuid not null,
  has_attachments boolean not null default false,
  preview         text not null default '',
  search_text     text not null default '',
  spam_score      real,
  spam_report     text,
  auth_results    jsonb,
  snoozed_until   timestamptz,               -- スヌーズ（この時刻まで一覧に出さない。時刻が来たら未読にして受信トレイの上に戻す）
  unique (mailbox_id, uid)
);
create index message_mailbox_date on message (mailbox_id, internal_date desc);
create index message_mailbox_modseq on message (mailbox_id, modseq);
create index message_thread on message (user_id, thread_id);
create index message_msgid on message (user_id, message_id);
create index message_blob on message (blob_id);
-- 重複の確認（同じ本体がすでにあるか。取り込み）
create index message_user_blob on message (user_id, blob_id);
-- 未読の数（メールボックスの一覧で毎回数える）と、古いものの掃除を速くする
create index message_unread on message (mailbox_id) where not ('\Seen' = any(flags));
create index message_internal_date on message (internal_date);
create index message_snoozed on message (snoozed_until) where snoozed_until is not null;
create index message_search on message using pgroonga (subject, from_addr, from_name, to_addrs, cc_addrs, search_text)
  with (normalizers = 'NormalizerNFKC150("unify_kana", true, "unify_to_romaji", false)');

-- 使用量（app_user.used_bytes）をメッセージの追加・削除に合わせて更新する
create function message_usage() returns trigger language plpgsql as $$
begin
  if tg_op = 'INSERT' then
    update app_user set used_bytes = used_bytes + new.size where id = new.user_id;
    return new;
  else
    update app_user set used_bytes = greatest(0, used_bytes - old.size) where id = old.user_id;
    return old;
  end if;
end $$;
create trigger message_usage after insert or delete on message for each row execute function message_usage();

-- 画面から送るときの添付ファイル（一時的）
create table upload (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  filename     text not null,
  content_type text not null,
  size         bigint not null,
  blob_id      text not null references blob(id),
  created_at   timestamptz not null default now()
);

-- 予約送信・送信の取り消し（送信はいったんここに置き、時刻が来たら送る。取り消しの猶予もこれで作る）
create table scheduled_send (
  id          uuid primary key,
  user_id     uuid not null references app_user(id) on delete cascade,
  send_at     timestamptz not null,
  req         jsonb not null,                 -- 作成画面の内容（ComposeReq）。取り消したら作成画面に戻す
  upload_ids  uuid[] not null default '{}',   -- 送るまで添付の一時保存を消さない
  ip          text,
  status      text not null default 'queued' check (status in ('queued', 'sending', 'sent', 'failed', 'cancelled')),
  error       text,
  created_at  timestamptz not null default now(),
  sent_at     timestamptz
);
create index scheduled_send_due on scheduled_send (send_at) where status = 'queued';
create index scheduled_send_user on scheduled_send (user_id, send_at);

-- アドレス帳（宛先として使ったので自動で覚えたものと、自分で保存した連絡先）
create table contact (
  user_id   uuid not null references app_user(id) on delete cascade,
  address   text not null,
  name      text,
  uses      int not null default 1,
  last_used timestamptz not null default now(),
  saved     boolean not null default false,  -- 自分で保存した（自動で覚えたものと分ける）
  company   text,
  phone     text,
  note      text,
  primary key (user_id, address)
);
create index contact_user_saved on contact (user_id, saved, lower(coalesce(name, address)));

-- 振り分けの規則
create table filter_rule (
  id         uuid primary key,
  user_id    uuid not null references app_user(id) on delete cascade,
  name       text not null,
  position   int not null,
  enabled    boolean not null default true,
  match_all  boolean not null default true,
  conditions jsonb not null,
  actions    jsonb not null,
  created_at timestamptz not null default now()
);
create index filter_rule_user on filter_rule (user_id, position);

-- 自動返信を送った相手（同じ相手には一定期間送らない）
create table vacation_sent (
  user_id uuid not null references app_user(id) on delete cascade,
  sender  text not null,
  sent_at timestamptz not null,
  primary key (user_id, sender)
);

-- ---------------------------------------------------------------- 配送

-- 外への送信待ち（宛先ごと）
create table outbound (
  id              uuid primary key,
  user_id         uuid references app_user(id) on delete set null,
  sender          text not null,           -- エンベロープの MAIL FROM（空は bounce）
  recipient       text not null,
  domain          text not null,
  blob_id         text not null references blob(id),
  status          text not null default 'queued', -- queued | sent | failed
  attempts        int not null default 0,
  next_attempt_at timestamptz not null default now(),
  last_error      text,
  created_at      timestamptz not null default now(),
  done_at         timestamptz
);
create index outbound_due on outbound (next_attempt_at) where status = 'queued';
create index outbound_created on outbound (created_at desc);
create index outbound_user_recent on outbound (user_id, created_at) where user_id is not null;

-- グレイリスト（初めての送信元・送信者・宛先の組み合わせを一度だけ一時拒否する）
create table greylist (
  key        text primary key,
  first_seen timestamptz not null default now(),
  passed     boolean not null default false
);

-- 受信の記録（統計・監視用。中身は持たない）
create table mail_log (
  id         bigserial primary key,
  at         timestamptz not null default now(),
  direction  text not null,                  -- in | out | submit
  result     text not null,                  -- delivered | junk | rejected | tempfail | sent | bounced
  ip         text,
  sender     text,
  recipients text,
  size       int,
  score      real,
  detail     text
);
create index mail_log_at on mail_log (at desc);

-- ---------------------------------------------------------------- 迷惑メールの学習（ベイズ）

-- user_id が 00000000-0000-0000-0000-000000000000 のものは全員の共有
create table bayes_token (
  user_id uuid not null,
  token   text not null,
  spam    int not null default 0,
  ham     int not null default 0,
  primary key (user_id, token)
);

create table bayes_total (
  user_id uuid primary key,
  spam    int not null default 0,
  ham     int not null default 0
);

-- ---------------------------------------------------------------- 運用

create table audit_log (
  id      bigserial primary key,
  at      timestamptz not null default now(),
  user_id uuid references app_user(id) on delete set null,
  via     text not null,
  action  text not null,
  target  text,
  ip      text,
  detail  jsonb not null default '{}'::jsonb
);
create index audit_log_at on audit_log (at desc);

create table job_lock (
  name       text primary key,
  holder     text not null,
  expires_at timestamptz not null
);

-- ---------------------------------------------------------------- 取り込み

-- ほかのサービス（Gmail など）からのメールの取り込み。
-- imap: サーバーが裏で取り込む（どれか1台のノードが引き受ける）。file: 画面が mbox・.eml を分けて送る
create table import_job (
  id          uuid primary key,
  user_id     uuid not null references app_user(id) on delete cascade,
  kind        text not null check (kind in ('imap', 'file')),
  status      text not null default 'queued' check (status in ('queued', 'running', 'done', 'failed', 'cancelled')),
  source      text not null,             -- 表示用（user@imap.gmail.com・ファイル名）
  secret      text,                      -- IMAP の接続情報（SecretBox で暗号化）。終わったら消す
  imported    int not null default 0,
  skipped     int not null default 0,    -- すでにある（重複）
  failed      int not null default 0,    -- 大きすぎる・容量の上限・壊れている
  current     text not null default '',  -- 処理中のフォルダ
  error       text,
  node        text,
  heartbeat   timestamptz,
  created_at  timestamptz not null default now(),
  finished_at timestamptz
);
create index import_job_user on import_job (user_id, created_at desc);
create index import_job_pending on import_job (status) where status in ('queued', 'running');

-- ---------------------------------------------------------------- DMARC の集計レポート

-- 受け取った DMARC の集計レポート（rua。dmarc-reports@<ドメイン> に届いたもの）
create table dmarc_report (
  id            uuid primary key,
  domain        text not null,
  org_name      text not null,
  email         text,
  report_id     text not null,
  policy        text not null,
  begin_at      timestamptz not null,
  end_at        timestamptz not null,
  total         bigint not null,
  passing       bigint not null,
  rows          jsonb not null,
  authenticated boolean not null,     -- 報告のメールが SPF か DKIM で認証されていたか（偽の報告で集計を汚されないように）
  received_at   timestamptz not null default now(),
  unique (domain, org_name, report_id)
);
create index dmarc_report_domain on dmarc_report (domain, end_at desc);

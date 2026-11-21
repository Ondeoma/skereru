-- 振り分けの規則の「添付を skawau に保存」の送信待ち（SKIP LOCKED で分担し、失敗したら間を空けて送り直す）
create table skawau_save (
  id          uuid primary key,
  user_id     uuid not null references app_user(id) on delete cascade,
  message_id  uuid not null,
  not_before  timestamptz not null default now(),
  attempts    int not null default 0,
  last_error  text,
  done_at     timestamptz,
  failed      boolean not null default false,
  created_at  timestamptz not null default now()
);
create index skawau_save_pending_idx on skawau_save (not_before) where done_at is null and not failed;
create index skawau_save_user_idx on skawau_save (user_id, created_at);

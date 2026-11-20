-- 使われなくなった本体（blob）を消すとき、アップロード・送信待ちから参照されていないかを本体ごとに確かめる。
-- blob_id から引けるようにする（外部キーの確認にも使われる）。
create index upload_blob on upload (blob_id);
create index outbound_blob on outbound (blob_id);
-- 利用者ごとの、まだ使っていないアップロードの数と大きさ（アップロード・送信のたびに数える）
create index upload_user on upload (user_id);

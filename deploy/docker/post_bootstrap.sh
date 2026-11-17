#!/bin/sh
# クラスタ初回作成時にプライマリで1度だけ実行される: アプリ用ロール・DB・PGroonga拡張を作る
set -eu
# パスワードを渡し忘れたら、既知の値で作らずに止める。値は psql の変数で渡し、:'pw' で正しくクオートする
: "${SKERERU_DB_PASSWORD:?SKERERU_DB_PASSWORD を設定してください}"
psql -h /tmp -U postgres -v ON_ERROR_STOP=1 -v pw="$SKERERU_DB_PASSWORD" <<'SQL'
create role skereru login password :'pw';
create database skereru owner skereru;
\c skereru
create extension if not exists pgroonga;
SQL

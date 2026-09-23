# 未知キー一覧の順序が未定義でテストが実行経路により失敗する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-map-keys-order
- Polished: YYYY-MM-DD

## 目的

`initialize/2,3` が返す未知キー一覧と `validate_options/1` のエラーが `maps:keys/1` の順序に依存しており、実行経路によって順序が変わる。テストが不安定になるのを防ぎ、返却順を決定的にする。

## 現状

- `kvconf` の `validate_options/1` は `{error, {unknown_option_keys, maps:keys(Options)}}` を返し、`unknown_keys/2` / `undoc_kv_list/2` も `maps:keys/1` の順序で返す
- `maps:keys/1` の順序は言語仕様上未定義。同じビームでも呼び出し経路で順序が変わった:
  - `./rebar3 as test eunit` → `validate_options_test` が成功
  - `erl -noshell -pa ... -eval 'eunit:test([kvconf], [])'` → 同じテストが `{expected, {unknown_option_keys, [spam, egg]}}` / `{value, {unknown_option_keys, [egg, spam]}}` で失敗
- README / CHANGES / issues は返却順を約束していない
- `issues/0005` は `unknown_keys` / `undoc_kv_list` の返り値形式の変更を伴うため、完了条件の「既存テストがそのまま通ること」と衝突する

## 設計方針

- `validate_options/1` / `unknown_keys/2` / `undoc_kv_list/2` の返却値を `lists:sort/1` で決定的にする
- テストは順序に依存しない比較 (`lists:sort/1` を挟む) に変更する
- `issues/0005` の完了条件と衝突するため、`0005` より先に対応するか、`0005` の完了条件を「ソート後に同じ要素が返ること」に調整する

## 完了条件

- rebar3 経由 / 素の erl 経由のどちらでも `validate_options_test` が通る
- 同じ入力に対して返却順が常に同一 (ソート順) になる
- 未知キーと `unknown_option_keys` の順序を検証するテストが追加されている

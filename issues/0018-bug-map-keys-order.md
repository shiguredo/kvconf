# 未知キー一覧の順序が未定義で実行方法によりテストが失敗する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-map-keys-order
- Polished: 2026-09-23

## 目的

`initialize/2,3` が返す未知キー一覧と `validate_options/1` のエラーが `maps:keys/1` の順序に依存しており、実行経路によって順序が変わる。テストが不安定になるのを防ぎ、返却順を決定的にする。

## 現状

- `kvconf` の `validate_options/1` は `{error, {unknown_option_keys, maps:keys(Options)}}` を返し、`unknown_keys/2` / `undoc_kv_list/2` も `maps:keys/1` の順序で返す
- `maps:keys/1` の順序は言語仕様上未定義。同一のビーム（`_build/test/lib/kvconf/ebin/kvconf.beam` を `code:which/1` で確認）を読み込んでいても、実行方法によって順序が変わった:
  - `./rebar3 as test eunit` → `validate_options_test` が成功
  - `erl -noshell -pa _build/test/lib/kvconf/ebin -eval 'eunit:test([kvconf], [])' -s init stop` → 同じテストが `{expected, {unknown_option_keys, [spam, egg]}}` / `{value, {unknown_option_keys, [egg, spam]}}` で失敗
- README / CHANGES / issues は返却順を約束していない
- `issues/0005` は返却順の決定性を本 issue に委ねており（毎回の atom 生成停止のみを扱い、期待順序は変更しない）、両者の完了条件は衝突しない。返り値の外側の `{ok, ...}` 化は `issues/0001`、`undoc_kv_list` の値の取得元と `KvcList` の扱いは `issues/0012` が担当する

## 設計方針

- `validate_options/1` / `unknown_keys/2` / `undoc_kv_list/2` の返却値を `lists:sort/1` で決定的にする（`undoc_kv_list/2` のキーは一意なので `lists:sort/1` と `lists:keysort(1, ...)` は同じ結果になる）
- テストは実装の保証（ソート順）を固定して検証する。既存の `validate_options_test` の期待値をソート後の値に更新し、`lists:sort/1` で緩めない
- `issues/0001`（返り値の `{ok, ...}` 化） / `0012`（`undoc_kv_list` の値の取得元と `KvcList` の扱い） / `0022`（死にコードと不要な設定の削除。`undoc_kv_list/3` の `KvcList` 引数は 0012 が使用するため 0022 の削除対象ではない）とは実装順に依存しない。ソートは外側のタプルではなく内側のリストに適用する

## 完了条件

- rebar3 経由 / 素の erl 経由のどちらでも `validate_options_test` が通る
- 同じ入力に対して返却順が常に同一 (ソート順) になる
- `validate_options_test` に未知オプションキー 2 件の順序検証が、`unknown_keys_test` に複数キー入力でソート済み順を検証するケースが追加されている（`undoc_kv_list_test` の順序検証は 0012 の書き換えに合わせる）
- 返却順が決定的になるユーザーに見える挙動変更（`unknown_keys` / `undoc_kv_list` / `unknown_option_keys` の並び）を伴うため、CHANGES.md の develop セクションに [FIX] として追記する

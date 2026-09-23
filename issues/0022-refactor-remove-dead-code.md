# 死にコードと不要な設定を削除する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/refactor-remove-dead-code
- Polished: YYYY-MM-DD

## 目的

参照されていない record・引数・節と、現行ツールチェーンで不要になった設定・コメントを削除する (Don't live with broken windows)。

## 現状

- `include/kvconf.hrl` の `#kvc_pkix_cacert_path{}` は参照 0 (`kvconf` の `type()` にも `validate_type/2` にもなく、`kvconf_pkix` の TODO に名前が出るだけ)
- `kvconf` の `undoc_kv_list/3` の `KvcList` 引数は全節で再帰に渡すだけで未使用
- `kvconf` の `key_to_env_name/2` の `undefined` 節は呼び出し元 (`maybe_env_overrides0/3`) が `undefined` を除外するため到達不能
- `include/kvconf.hrl` の `-include_lib("eunit/include/eunit.hrl")` はヘッダ内で未使用で、include する 3 ファイル (`kvconf` / `kvconf_validate` / `test/kvconf_tests`) が自前で include している
- `.gitignore` の `/.efmt/*` は efmt 0.21.x がキャッシュを作らないため不要 (ローカルの `.efmt` は 2022 年の旧キャッシュのみ)。`_build` は `_*` と重複
- `rebar.config` のコメントアウトされた `eqwalizer_support` (1 年以上放置) と dialyzer の警告オプション群
- 放置コメント: `kvconf` の parse 定数化 TODO、`undoc_kv_list` / `unknown_keys` の効率 XXX、`kvconf_pkix` の実装予定のない TODO

## 設計方針

- 参照 0 / 到達不能を確認したものだけ削除する
- `eqwalizer_support` は残すなら issue 化し、dialyzer の警告オプションは選択肢の記録として残すなら理由コメントを付ける
- `time_unit/1` の `us` 節は `min` / `max` に `us` を書いた場合に到達するため削除しない

## 完了条件

- `make compile test dialyzer` と `prek run --all-files` が通る
- 削除した項目が解決方法に列挙されている

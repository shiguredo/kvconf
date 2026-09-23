# 死にコード・不要な設定・放置コメントを削除する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/refactor-remove-dead-code
- Polished: 2026-09-23

## 目的

参照されていない record・節と、現行ツールチェーンで不要になった設定・コメントを削除する (Don't live with broken windows)。

## 現状

- `include/kvconf.hrl` の `#kvc_pkix_cacert_path{}` は参照 0 (`kvconf` の `type()` にも `kvconf_validate` の `validate_type/2` にも節がなく、`kvconf_pkix` の TODO に名前が出るだけ)。同じ record に紐づく `include/kvconf.hrl` の `%% TODO: dir を指定できる、未実装` と `kvconf_pkix` の `%% TODO: 複数 CA ファイル設定できる vlaidate_pkix_cacert_path ...` も宙に浮く
- `kvconf` の `key_to_env_name/2` の `undefined` 節は呼び出し元 (`maybe_env_overrides/3`) が `undefined` を除外するため到達不能 (`-spec` の `binary() | undefined` も合わせて更新する)
- `include/kvconf.hrl` の `-include_lib("eunit/include/eunit.hrl")` はヘッダ内で未使用で、include する 3 ファイル (`kvconf` / `kvconf_validate` / `test/kvconf_tests`) が自前で include している (`issues/0021` の `prop_*` モジュールが `proper.hrl` と同時に include するために、この削除を前提にしている)
- `.gitignore` の `/.efmt/*` は efmt 0.21.x (prek 経由の erlang-pre-commit 2026.4.0) がキャッシュを作らないため不要 (ローカルの `.efmt` は 2022 年の旧キャッシュのみ)。`_build` は `_*` と重複
- `rebar.config` のコメントアウトされた `eqwalizer_support` は 2025.1.0 のリリース時に「一時的にコメントアウト」されたまま、現行の CI / Makefile / prek のどこからも実行されていない。dialyzer の警告オプションの候補 (`unmatched_returns` / `overspecs` / `underspecs` / `specdiffs` / `error_handling`) と `{plt_apps, all_apps}` も無効化の理由がコードにも記録にない
- `undoc_kv_list/3` の `KvcList` 引数は現行では全節で再帰に渡すだけで未使用だが、`issues/0012` が「Configurations に存在し KvcList に存在する undoc_ キーのみ返す」判定にこの引数を使うため、削除対象にしない (0012 実装後は使用される)

## 設計方針

- 削除対象は (1) 参照 0 の record とそれに紐づく TODO コメント、(2) 到達不能な節、(3) 旧ツールチェーン向けの設定と ignore、(4) 実行されていないコメントアウト設定に限る
- `kvconf` の parse 定数化 TODO・`undoc_kv_list` / `unknown_keys` の効率 XXX と、`kvconf_pkix` の複数証明書・PKI Asn1Type の TODO は将来の改善メモなので削除しない
- `rebar.config` のコメントアウトされた `eqwalizer_support`、dialyzer の警告オプション 5 種、`{plt_apps, all_apps}` は削除する (再有効化する場合は git 履歴から復元する)
- `time_unit/1` の `us` 節の扱いは `issues/0017` の実装状況で決める。0017 を先に実装した場合は `min` / `max` の単位が `?IN_TIME_UNIT` に限定され到達不能になるため削除対象に含める。0017 未実装のまま本 issue を実施する場合は `min = {10, us}` で到達するため削除しない
- 公開ヘッダから record と eunit の include を外すため、CHANGES.md の develop の `### misc` に [CHANGE] として追記する
- `.efmt` を削除した状態で `prek run --all-files` を実行し、`.efmt` が再生成されないことを確認する

## 完了条件

- `make compile test dialyzer` と `prek run --all-files` が通る
- `.efmt` を削除した状態で `prek run --all-files` を実行しても `.efmt` が生成されない
- 削除・変更した項目 (`#kvc_pkix_cacert_path{}` とそれに紐づく TODO 2 件、`key_to_env_name/2` の `undefined` 節と `-spec`、`include/kvconf.hrl` の eunit include、`/.efmt/*`、`_build` の重複行、`eqwalizer_support` のコメントアウト、dialyzer の警告オプション 5 種、`{plt_apps, all_apps}`) を完了時に列挙する (削除しなかった項目とその理由も含める)
- CHANGES.md の develop の `### misc` に削除した項目を [CHANGE] として追記する

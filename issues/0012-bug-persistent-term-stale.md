# initialize の検証失敗時やキーが設定から外れた場合に persistent_term の stale 値が残るのと、UndocKvList が残留値を返すのを修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-persistent-term-stale
- Polished: 2026-07-31

## 目的

initialize の検証失敗時やキーが設定から外れた場合に、persistent_term に古い値が残り続け、get_value が stale 値を返す。また UndocKvList が設定ファイルではなく persistent_term の値を返す。この状態管理を修正する。

## 現状

- kvconf_validate の validate は検証成功したキーから順に kvconf:set_value で書き込むため、途中でエラーになると部分書き込みが残る
- required = false / default = undefined のキーは skip され何も消さないため、前回の initialize の値が残り続ける
- kvconf の undoc_kv_list は undoc_ キーの値を設定ファイルから読まず persistent_term（get_value）から読むため、set_value 直書きや前回 initialize の残留値が UndocKvList に載る
- 変更対象は src/kvconf.erl（initialize / undoc_kv_list）と src/kvconf_validate.erl（validate）

## 設計方針

- フローを「KvcList の全キーを unset → 全キーを検証 → 全キーの検証が成功した後に一括で set_value」に変更する。unset は parse 成功後・validate の前に実施し、検証失敗時も unset 済みの状態になる。parse エラー時は persistent_term を変更しない（現状どおり）
- validate は検証済み値を accumulate して返す形に変える（例: {ok, [{Key, ValidatedValue}]} | {error, ...}）。initialize 側で成功後に一括 set_value する。エラー形式 {error, {Reason, Line, LineNumber}} は不変
- unset 対象は KvcList の全キー（設定ファイルから外れたキーの stale 値を消す。required = false / default = undefined で skip されるキーも含む）
- KvcList から外れたキー（前回の KvcList にあったが今回の KvcList に無いキー）は、initialize が前回の KvcList を保持していないため検出できず、本 issue の対象外とする
- UndocKvList は Configurations に存在する undoc_ キーのみを返し、値は検証済み値（persistent_term）を返す。set_value 直書きや前回 initialize の残留値が載らない
- KvcList に無い undoc_ キーは返さない（0005 の設計方針と完了条件の「KvcList に無い undoc_ キーでも persistent_term に値があれば返す（現行どおり）」という記述は、本 issue の実装時に「KvcList に無い undoc_ キーは返さない」に更新することで調整する）
- 0001 は undoc_kv_list の返り値形式の {ok, ...} 化を担当し、本 issue は値の取得元を担当する。undoc_kv_list_test の書き換えは 0001 の {ok, ...} 化を前提とした形式で本 issue が実施する。環境変数による上書き後の値（Configurations は env 上書き後の状態）も含む

## 完了条件

- 検証失敗直後に、KvcList の全キー（default 付きキーを含む）について get_value が not_found を返す（default の set は全キーの検証成功後に一括で行われるため、検証失敗時は set されない）
- キーが設定ファイルに記述されなくなった後（required = false / default = undefined で skip される場合を含む）に、default が設定されていないキーでは get_value が not_found を返す（default 付きキーは検証成功時に一括 set で default 値が書き込まれるため default が返る）
- UndocKvList が検証済み値（set_value 直書きや残留値を含まない）を返す
- 回帰テストが test/kvconf_tests.erl に追加されている（initialize の成功 → 失敗 → 再成功のシーケンスで stale 値が残らないこと、検証失敗直後に KvcList の全キーで get_value が not_found になること、キーが設定ファイルから外れた後の not_found、set_value 直書きが UndocKvList に載らないこと。undoc_kv_list_test は persistent_term 前提のため書き換えが必要）

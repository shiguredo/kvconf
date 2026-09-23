# initialize の検証失敗時やキーが設定から外れた場合に persistent_term の stale 値が残るのと、UndocKvList が残留値を返すのを修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-persistent-term-stale
- Polished: 2026-09-23

## 目的

initialize の検証失敗時やキーが設定から外れた場合に、persistent_term に古い値が残り続け、get_value が stale 値を返す。また UndocKvList が、設定ファイルの値ではなく set_value の直書き値や前回 initialize の残留値（persistent_term）を返す。この状態管理を修正する。

## 現状

- kvconf_validate の validate は検証成功したキーから順に kvconf:set_value で書き込むため、途中でエラーになると部分書き込みが残る
- required = false / default = undefined のキーは skip され何も消さないため、前回の initialize の値が残り続ける
- kvconf の undoc_kv_list は Configurations に undoc_ キーがある場合に、設定ファイルの値ではなく persistent_term（get_value）から値を読むため、set_value 直書きや前回 initialize の残留値が UndocKvList に載る（実測: Configurations に undoc_foo = 7 があり persistent_term に undoc_foo = 999 を直書きすると [{undoc_foo, 999}] を返す）
- 変更対象は src/kvconf.erl（initialize / undoc_kv_list）と src/kvconf_validate.erl（validate）

## 設計方針

- フローを「KvcList の全キーを unset → 全キーを検証 → 全キーの検証が成功した後に一括で set_value」に変更する。unset は parse 成功後・validate の前に実施し、検証失敗時も unset 済みの状態になる。parse エラー時は persistent_term を変更しない（現状どおり）
- validate は検証済み値を accumulate して返す形に変える（例: {ok, [{Key, ValidatedValue}]} | {error, ...}）。initialize 側で成功後に一括 set_value する。エラー形式 {error, {Reason, Line, LineNumber}} は不変
- unset 対象は KvcList の全キー（設定ファイルから外れたキーの stale 値を消す。required = false / default = undefined で skip されるキーも含む）
- KvcList から外れたキー（前回の KvcList にあったが今回の KvcList に無いキー）は、initialize が前回の KvcList を保持していないため検出できず、本 issue の対象外とする
- UndocKvList の取得元は persistent_term のままでよい。unset と全キー検証成功後の一括 set により persistent_term は設定ファイル（env 上書き後）の検証済み値と一致するため、set_value 直書きや前回 initialize の残留値は載らない。判定は Configurations に存在し、かつ KvcList に存在する undoc_ キーに限る
- KvcList に無い undoc_ キーは返さない（検証されず persistent_term に検証済み値が無いため、set_value 直書きの値が載るのを防ぐ。0005 は「Configurations に存在する undoc_ キーのみを返す」という必要条件を記載しており矛盾しない。この KvcList 判定により 0022 の「undoc_kv_list の KvcList 引数は未使用」という前提は本 issue の実装で成立しなくなる）
- 0001 は undoc_kv_list の返り値形式の {ok, ...} 化を担当し、本 issue は値の取得元を担当する。0001 も initialize/3 の maybe チェーンと src/kvconf.erl の undoc_kv_list_test を変更するため、本 issue は 0001 の実装後に着手する。undoc_kv_list_test の書き換えは 0001 の {ok, ...} 化を前提とした形式で本 issue が行う。環境変数による上書き後の値（Configurations は env 上書き後の状態）も含む

## 完了条件

- 検証失敗直後に、KvcList の全キー（default 付きキーを含む）について get_value が not_found を返す（default の set は全キーの検証成功後に一括で行われるため、検証失敗時は set されない）
- キーが設定ファイルに記述されなくなった後（required = false / default = undefined で skip される場合を含む）に、default が設定されていないキーでは get_value が not_found を返す（default 付きキーは検証成功時に一括 set で default 値が書き込まれるため default が返る）
- UndocKvList が検証済み値（set_value 直書きや残留値を含まない）を返す
- 回帰テストが test/kvconf_tests.erl に追加されている（initialize の成功 → 失敗 → 再成功のシーケンスで stale 値が残らないこと、検証失敗直後に KvcList の全キーで get_value が not_found になること、キーが設定ファイルから外れた後の not_found、Configurations に存在し KvcList に無い undoc_ キーに persistent_term の直書き値（Configurations の値とは別値）があるときに UndocKvList に載らないこと）
- src/kvconf.erl の undoc_kv_list_test が書き換えられている（0001 の {ok, ...} 化に追従し、KvcList に無い undoc_ キーが除外される期待値を追加する）
- 検証失敗時に get_value が stale 値を返さなくなる（not_found になる）ユーザーに見える挙動変更を伴うバグ修正のため、CHANGES.md の develop セクションに [FIX] として追記する

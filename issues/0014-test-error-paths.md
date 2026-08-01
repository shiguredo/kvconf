# エラーパス・境界値・統合経路のテストを追加する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/add-error-path-tests
- Polished: 2026-07-31

## 目的

parse のエラーパス、環境変数上書きの失敗系、境界値の検証が不足しており、実バグが混入しても検出できない。主要ロジックのテストを追加する。

## 現状

- kvconf の parse_lines の duplicated_key / invalid_line_format の分岐が未テスト（0006 は CRLF / タブ、0010 は空キー行の回帰テストを追加するが、duplicated_key と汎用の invalid_line_format は本 issue の担当）
- 環境変数上書きの失敗系（{error, {invalid_value, <<"ENV:...">>, 0}} 経路）が未テスト（0011 の完了条件により、失敗系テストは空白を含まない不正値を対象とし、前後空白・空白のみのケースは 0011 が担当する。前後空白 + 不正値（例: " 8080x "）は trim 後に不正値になるため本 issue の対象に含める）
- validate_integer のテストが 1 アサーションのみで、infinity / 範囲外が未検証（max = infinity / 範囲外のテストは 0009 の完了条件で 0014 の担当とされている。undefined / atom のテストは 0009 が担当する）
- validate_float の infinity / 範囲外が未検証（validate_float_test の新規作成は 0009 の完了条件で担われ、本 issue は infinity / 範囲外のケースを追加する。validate_float_test の二重作成を避けるため 0009 実装後に着手する）
- smoke_test で interval_ms の変換結果がアサーションされていない（期待値は 50。smoke_test.conf の interval_ms = 50 ms と out_time_unit = millisecond から導出）
- #kvc_list_atom / #kvc_list_string が initialize 統合経路（validate_type のディスパッチ）で未検証
- 不正 IPv4 / IPv6 文字列のエラーパスが未テスト

## 設計方針

- 上記の各経路にテストを追加する
- smoke_test に interval_ms の値（50 になること）のアサーションを追加する
- validate_interval_test の min 超過と単位混在の max 境界ケースは 0002 の完了条件で追加されるため、本 issue では扱わない（0002 実装後に存在することを確認する）
- テストの配置: parse_lines のテストは kvconf.erl の -ifdef(TEST) ブロック、validate_integer / validate_float のテストは kvconf_validate.erl の -ifdef(TEST) ブロック、環境変数上書きの失敗系・smoke_test・統合経路のテストは test/kvconf_tests.erl
- duplicated_key のテスト入力は非空キー（"a = 1\na = 2" 等）で構成する（空キーの重複は 0010 の修正後に挙動が変わるため）
- 不正 IPv4 文字列の例: <<"192.168.0.999">>、不正 IPv6 文字列の例: <<"2001:db8::zzzz">> が invalid_value になること
- #kvc_list_atom / #kvc_list_string の統合経路は、smoke_test.conf に対応行を追加し、smoke_test の KvcList に定義を追加して get_value の検証値をアサーションする（KvcList に追加するだけでは validate_one が skip され validate_type が走らないため、設定行とアサーションの両方が必要）

## 完了条件

- parse_lines の duplicated_key が {error, {duplicated_key, ...}} を返すことをテストで検証する
- parse_lines の invalid_line_format（= を含まない行等）が {error, {invalid_line_format, ...}} を返すことをテストで検証する
- 環境変数上書きの失敗系（空白を含まない不正値、例: "8080x" 等。前後空白 + 不正値も含む）が {error, {invalid_value, <<"ENV:...">>, 0}} を返すことをテストで検証する
- validate_integer / validate_float の infinity と範囲外のテストが追加されている（undefined / atom は 0009 の担当。min / max ちょうどの値が ok になるアサーションも含める）
- smoke_test に interval_ms の値が 50 であることのアサーションが追加されている
- #kvc_list_atom / #kvc_list_string が initialize 統合経路で検証される（smoke_test.conf への設定行追加と get_value のアサーション）
- 不正 IPv4 / IPv6 文字列が invalid_value になることをテストで検証する
- 0002 実装後に validate_interval_test の min 超過・単位混在 max 境界テストが存在することを確認する
- 上記の全経路がテストでカバーされる

# 環境変数の上書き値を trim する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-trim-env-value
- Polished: 2026-07-31

## 目的

環境変数による上書き値が trim されず、ファイル値と非対称な挙動になる。環境変数値にも誤って前後空白が混入し得るため、ファイル値と同じ挙動に揃える。

## 現状

- kvconf の maybe_env_overrides0 は os:getenv の値を list_to_binary でそのまま設定に追加する
- ファイル値は parse_lines で string:trim されるが、環境変数値は trim されない
- 実測: #kvc_port_number で環境変数 SPAM_PORT に " 8080 " を設定すると {error, {invalid_value, ...}} になる（ファイルなら 8080 として受理される）
- 0001（クラッシュ経路のエラー返却化）の経路 3 は parse_lines の string:trim の例外を保護するが、環境変数経路は対象外

## 設計方針

- maybe_env_overrides0 で os:getenv が返すリストのまま string:trim を適用してから list_to_binary する（binary 変換後の trim は不正 UTF-8 で error:{badarg, Binary} を投げるためクラッシュ経路を新規導入する。リストのまま trim すればクラッシュしない）
- 0001 の経路 3 は parse_lines のみを対象とするため、環境変数経路の trim のクラッシュ保護は本 issue の方式（リストのまま trim）で回避する

## 完了条件

- #kvc_integer / #kvc_port_number で環境変数値 " 8080 " が 8080 として受理される
- #kvc_string で環境変数値 " hello " が <<"hello">> になる
- 空白のみの環境変数値は trim 後 <<>> になり、#kvc_string では <<>> として受理され、#kvc_integer / #kvc_port_number では invalid_value になる（回帰テストで検証する）
- 回帰テストが test/kvconf_tests.erl の環境変数上書きテスト群に追加されている（前後空白と空白のみのケースは本 issue が担当し、0014 の失敗系テストは空白を含まない不正値を対象とする）

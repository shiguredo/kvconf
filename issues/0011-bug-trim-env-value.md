# 環境変数の上書き値を trim する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-trim-env-value
- Polished: 2026-09-23

## 目的

環境変数による上書き値が trim されず、ファイル値と非対称な挙動になる。環境変数値にも誤って前後空白が混入し得るため、ファイル値と同じ挙動に揃える。

## 現状

- kvconf の maybe_env_overrides0 は os:getenv の値を list_to_binary でそのまま設定に追加する
- ファイル値は parse_kv_line/4 で string:trim されるが、環境変数値は trim されない
- 実測: #kvc_port_number で環境変数 SPAM_PORT に " 8080 " を設定すると {error, {invalid_value, ...}} になる（ファイルなら 8080 として受理される）
- 0001（クラッシュ経路のエラー返却化）の経路 3 は parse_kv_line/4 の string:trim の例外を保護するが、環境変数経路は対象外
- os:getenv は native_name_encoding が utf8 の環境では非 ASCII 値を codepoint リストで返すため、現行の list_to_binary は codepoint が 255 を超える値（例: "日本語"）で badarg クラッシュし、255 以下でも latin1 バイト列に変換されて UTF-8 が壊れる（実測: os:getenv が [26085, 26412, 35486] を返し list_to_binary が badarg）。この環境変数値のエンコーディング変換は trim とは独立した既存の欠陥であり、本 issue では扱わない（list_to_binary は変更しない）

## 設計方針

- maybe_env_overrides0 で os:getenv が返すリストのまま string:trim を適用してから list_to_binary する（binary 変換後の trim は不正 UTF-8 で error:{badarg, Binary} を投げるため、trim による新たなクラッシュ経路を持ち込まない。list_to_binary 自体のエンコーディング問題は現状に記載したとおり本 issue の対象外）
- 0001 の経路 3 は parse_kv_line/4 のみを対象とするため、環境変数経路の trim のクラッシュ保護は本 issue の方式（リストのまま trim）で回避する

## 完了条件

- #kvc_integer / #kvc_port_number で環境変数値 " 8080 " が 8080 として受理される
- #kvc_string で環境変数値 " hello " が <<"hello">> になる
- 空白のみの環境変数値は trim 後 <<>> になり、#kvc_string では <<>> として受理され、#kvc_integer / #kvc_port_number では invalid_value になる（回帰テストで検証する）
- 回帰テストが test/kvconf_tests.erl の環境変数上書きテスト群に追加されている（前後空白付きの正常値と空白のみのケースは本 issue が担当し、前後空白 + 不正値（例: " 8080x "）を含む不正値の失敗系は 0014 が担当する）
- 環境変数値の前後空白が除去されるユーザーに見える挙動変更（例: #kvc_string で " hello " が <<"hello">> になる）であるため、CHANGES.md の develop セクションに [FIX] として追記する

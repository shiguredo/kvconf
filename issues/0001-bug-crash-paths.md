# initialize のクラッシュ経路をエラー返却に修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-crash-paths
- Polished: 2026-07-31

## 目的

kvconf:initialize/2 と initialize/3 は失敗時に {error, term()} を返す契約だが、一部の入力で例外（function_clause / badarg 等）が漏れて呼び出しプロセスをクラッシュさせる。すべてエラータプルとして返すように修正する。

## 現状

以下の 6 経路でクラッシュする（すべて実機で再現確認済み）:

1. #kvc_interval の min / max / out_time_unit 未設定
   - include/kvconf.hrl の #kvc_interval レコードは min / max / out_time_unit にデフォルト値を持たない
   - 未設定のまま使用すると kvconf_validate の validate_interval_min / validate_interval_max が function_clause を、validate_interval_out_unit は badarg を投げる
   - binary 経路は validate_interval 内の try...catch error:badarg で包まれており badarg は invalid_value 化するが、function_clause は漏れてクラッシュする。default（tuple）経路には try-catch がないため badarg も漏れる
2. default 値の型不正
   - validate_ipv4_address / validate_ipv6_address / validate_http_uri / validate_list_atom / validate_list_ipv4_address / validate_list_ipv6_address / validate_interval が binary 以外の入力（atom / integer 等）で badarg または function_clause を投げる
   - リスト要素が binary / tuple でない場合（例: #kvc_list_ipv4_address の default に atom を含むリスト）も同様にクラッシュする
   - kvconf_pkix の validate_pkix_fullchain_pem_file / validate_pkix_privkey_pem_file / validate_pkix_cert_pem_file も default がファイルパス（binary / string）以外の場合 file:read_file が badarg を投げる
3. 不正 UTF-8 バイト列を含む設定ファイル
   - kvconf の parse_lines 内の string:trim が badarg を投げる（OTP 29 では {badarg, Binary} タプル形式の例外）
   - badarg になるのはキー・値の先頭に不正バイトがある場合（例: 値が <<"a = \xFF">> の行）。文字列中間や末尾の不正バイトでは string:trim はクラッシュしない（キー末尾の不正バイトは経路 6 の binary_to_atom でクラッシュする）
4. 不正 base64 を含む PEM ファイル
   - kvconf_pkix の validate_pkix_fullchain_pem_file / validate_pkix_privkey_pem_file / validate_pkix_cert_pem_file 内の public_key:pem_decode が missing_padding / function_clause などの例外を投げる（base64 長が 4 の倍数でない、パディング不正、END 行がない等）
5. 高バイト（孤立 UTF-8 バイト）を含む URL
   - validate_http_uri 内の uri_string:parse が function_clause を投げる（ファイル・環境変数・default 経由で到達する）
6. 不正 UTF-8 を含むキー名
   - kvconf の unknown_keys / undoc_kv_list 内の binary_to_atom が badarg を投げる（例: キーが <<"a\xFFb">> の行）
   - 0005（unknown_keys / undoc_kv_list の binary_to_atom 廃止）と対象箇所が重なる。0005 がバイナリ比較を採用すれば解消されるが、binary_to_existing_atom 案では解消されないため本 issue でも保護する

## 設計方針

- 各バリデータの入力を型ガードと catch-all 節で保護し、不正入力は invalid_value を返す
- #kvc_interval の min / max / out_time_unit が undefined の場合に invalid_value を返す（available_time_units の undefined はデフォルトで合法なので対象外。また available_time_units の型不正（undefined でも list でもない値）は本 issue の対象外とする。KvcList のレコード定義はアプリ開発者の静的データであり実行時入力ではないため）
- validate_interval の maybe + else（false / error）構造を踏まえ、min / max の undefined 検知は error を返す節で行う（invalid_value を返す catch-all 節を追加すると ?= が else_clause を投げる）
- validate_interval_out_unit は maybe の最後の式であり ?= で検査されない。error を返す節では else 節が評価されず error が漏れるため、undefined 検知は validate_interval 側の分岐で行うか、{ok, ConvertedValue} ?= validate_interval_out_unit(...) と ?= 化する（validate_interval_out_unit は {ok, non_neg_integer()} を返すので ok ?= では else_clause になる）
- kvconf_pkix の validate_pkix_fullchain_pem_file / validate_pkix_privkey_pem_file / validate_pkix_cert_pem_file は関数全体（file:read_file の badarg を含む）を try-catch（catch _:_）で包み error を返す
- parse_lines は string:trim の例外（{badarg, Binary} タプル形式を含む）を {error, {invalid_line_format, ...}} に変換する
- validate_http_uri は uri_string:parse の例外（function_clause 等）を try-catch で捕捉し invalid_value を返す
- unknown_keys / undoc_kv_list の binary_to_atom は例外をエラー返却に変換する。unknown_keys / undoc_kv_list は現状 initialize 内で値代入されるだけの関数（kvconf の initialize）のため、呼び出し側を {ok, UnknownKeys} ?= unknown_keys(...) / {ok, UndocKvList} ?= undoc_kv_list(...) のように ?= 化し、{error, ...} を initialize のエラー返却に反映する
- kvconf_validate の validate_interval_max / validate_http_uri や kvconf の parse_lines は 0002 / 0006 / 0007 / 0010 と修正箇所が重なる。本 issue は例外のエラー返却化のみを行い、バリデーションロジックの変更は各 issue に委ねる

## 完了条件

- 上記 6 経路の入力で initialize がクラッシュせず {error, ...} を返す
  - 経路 1 / 2 / 5 は {error, {invalid_value, ...}} を返す（経路 2 の kvconf_pkix 系（file:read_file の badarg）は {error, {error, ...}} を返す）
  - 経路 3 は {error, {invalid_line_format, ...}} を返す
  - 経路 4 は {error, {error, ...}} を返す
  - 経路 6 は {error, {invalid_key_name, Key, LineNumber}} を返す
- 各経路の回帰テストが追加されている
- unknown_keys / undoc_kv_list の返り値形式の変更に伴い、kvconf の unknown_keys_test / undoc_kv_list_test が更新されている

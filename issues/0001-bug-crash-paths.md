# initialize のクラッシュ経路をエラー返却に修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-crash-paths
- Polished: 2026-09-23

## 目的

kvconf:initialize/2 と initialize/3 は失敗時に {error, term()} を返す契約だが、一部の入力で例外（function_clause / badarg 等）が漏れて呼び出しプロセスをクラッシュさせる。すべてエラータプルとして返すように修正する。

## 現状

以下の 7 経路でクラッシュする（すべて実機で再現確認済み）:

1. #kvc_interval の min / max / out_time_unit 未設定
   - include/kvconf.hrl の #kvc_interval レコードは min / max / out_time_unit にデフォルト値を持たない
   - 未設定のまま使用すると kvconf_validate の validate_interval_min / validate_interval_max が function_clause を、validate_interval_out_unit は badarg を投げる
   - binary 経路は validate_interval 内の try...catch error:badarg で包まれており badarg は invalid_value 化するが、function_clause は漏れてクラッシュする。default（tuple）経路には try-catch がないため badarg も漏れる
2. default 値の型不正
   - validate_ipv4_address / validate_ipv6_address / validate_http_uri / validate_list_atom / validate_list_ipv4_address / validate_list_ipv6_address / validate_interval が binary 以外の入力（atom / integer 等）で badarg または function_clause を投げる
   - リスト要素が binary / tuple でない場合（例: #kvc_list_ipv4_address の default に atom を含むリスト）も同様にクラッシュする
   - kvconf_pkix の validate_pkix_fullchain_pem_file / validate_pkix_privkey_pem_file / validate_pkix_cert_pem_file は file:read_file/1 が例外を投げず {error, Reason} を返すため、default がファイルパス以外（atom / integer 等）でもクラッシュせず error を返す（実測: file:read_file(1) は {error, badarg}）。この経路は修正不要で、回帰確認のみ行う
3. 不正 UTF-8 バイト列を含む設定ファイル
   - kvconf の parse_kv_line/4 内の string:trim が badarg を投げる（OTP 29 では {badarg, Binary} タプル形式の例外）
   - badarg になるのは、キー・値の先頭に不正バイトがある場合（例: ファイル内容が <<"a = \xFF">> の行）と、ASCII 文字と空白の後ろに不正バイト列（切り詰めを含む）が続く場合（例: <<"a = b ", 16#C3>> の行）である。文字列の中間や末尾に直接続く不正バイト（例: <<"a = b\xFF">>）ではクラッシュしない（キー末尾の不正バイトは経路 6 の binary_to_atom でクラッシュする）
4. 不正 base64 を含む PEM ファイル
   - kvconf_pkix の validate_pkix_fullchain_pem_file / validate_pkix_privkey_pem_file / validate_pkix_cert_pem_file 内の public_key:pem_decode が missing_padding / function_clause などの例外を投げる（base64 長が 4 の倍数でない、パディング不正、END 行がない等）
5. 高バイト（孤立 UTF-8 バイト）を含む URL
   - validate_http_uri 内の uri_string:parse が function_clause を投げる（ファイル・環境変数・default 経由で到達する）
6. 不正 UTF-8 または長すぎるキー名
   - kvconf の unknown_keys / undoc_kv_list 内の binary_to_atom が badarg を投げる（例: キーが <<"a\xFFb">> の行）
   - atom の最大長は 255 文字であり、256 文字以上のキーでは binary_to_atom が system_limit を投げる（実測: ASCII 256 文字で system_limit、UTF-8 150 文字 / 300 バイトは成功。バイト数ではなく文字数で判定される）
   - 0005（unknown_keys / undoc_kv_list の binary_to_atom 廃止）は unknown_keys をバイナリ比較、undoc_kv_list を binary_to_existing_atom + catch スキップにするため、0005 の実装後はこの経路で例外は発生しない。本 issue は 0005 の実装順に依存せず現行コードでもクラッシュしないことを保証するため、現行の binary_to_atom を try-catch で保護する（0005 実装後はこの保護は発火しない）
7. env_prefix の不正 UTF-8
   - Options の env_prefix に不正 UTF-8 バイナリを渡すと、kvconf の key_to_env_name/2 内の string:uppercase/1 が badarg を投げる（実測: string:uppercase(<<16#FF>>) は {badarg, <<16#FF>>}）
   - validate_options/1 は env_prefix が binary かつ空でないことだけを検査し、UTF-8 妥当性を見ていない。maybe_env_overrides0/3 が KvcList の各キーに対して key_to_env_name/2 を呼ぶため、KvcList が空でなければ到達する

## 設計方針

- 各バリデータの入力を型ガードと catch-all 節で保護し、不正入力は invalid_value を返す（kvconf_pkix は error を返し、initialize 経由では {error, {error, ...}} になる）
- #kvc_interval の min / max / out_time_unit が undefined の場合に invalid_value を返す（available_time_units の undefined はデフォルトで合法なので対象外。available_time_units の型不正は 0017 が担当するため本 issue の対象外）
- validate_interval_min/2 と validate_interval_max/2 に第 2 引数が undefined の場合に error を返す節を追加する（validate_interval の maybe は ok ?= で検査しており、invalid_value を返す節を追加すると else_clause を投げるため error を返す）
- validate_interval_out_unit は maybe の最後の式で ?= に載らないため、out_time_unit の undefined は validate_interval/2 の maybe 内に true ?= (OutUnit =/= undefined) を追加して検知する（else の false 節が invalid_value を返す）
- kvconf_pkix の validate_pkix_fullchain_pem_file / validate_pkix_privkey_pem_file / validate_pkix_cert_pem_file は関数全体を try-catch（catch _:_）で包み、public_key:pem_decode/1 の例外（missing_padding / function_clause 等）を error に変換する。この try-catch は 0003 が追加する public_key:der_decode/2 の例外もカバーする。file:read_file/1 は例外を投げず {error, Reason} を返すため、default がファイルパス以外でも現状の分岐で error になる（非パス入力の回帰テストのみ追加する）
- parse_kv_line/4 は string:trim の例外（{badarg, Binary} タプル形式を含む）を {error, {invalid_line_format, ...}} に変換する
- validate_http_uri は uri_string:parse の例外（function_clause 等）を try-catch で捕捉し invalid_value を返す
- unknown_keys / undoc_kv_list の binary_to_atom を try-catch で保護し、例外時は invalid_key_name を返す。呼び出し側を {ok, UnknownKeys} ?= unknown_keys(...) / {ok, UndocKvList} ?= undoc_kv_list(...) のように ?= 化し、{error, ...} を initialize のエラー返却に反映する。LineNumber は Configurations の値タプル {Value, Line, LineNumber} から取得する。0005 の実装後は両関数とも例外を返さなくなるためこの保護は発火しないが、実装順に依存しないための保険として残す
- validate_options/1 で env_prefix を string:uppercase/1 に通して badarg を捕捉し、不正な UTF-8 なら {error, {invalid_option_value, env_prefix, Value}} を返す（key_to_env_name/2 では捕捉しない）
- kvconf_validate の validate_interval_max / validate_http_uri や kvconf の parse_kv_line は 0002 / 0006 / 0007 / 0010 と修正箇所が重なる。本 issue は例外のエラー返却化のみを行い、バリデーションロジックの変更は各 issue に委ねる
- リスト型バリデータの improper list（default / candidates）は 0023、#kvc_interval の単位・型の検証は 0017、validate_list_atom の binary 経路は 0016 で扱う

## 完了条件

- 上記 7 経路の入力で initialize がクラッシュせず {error, ...} を返す
  - 経路 1 / 2 / 5 は {error, {invalid_value, ...}} を返す（経路 2 の kvconf_pkix 系は現状から {error, {error, ...}} を返すため回帰確認）
  - 経路 3 は {error, {invalid_line_format, ...}} を返す
  - 経路 4 は {error, {error, ...}} を返す
  - 経路 6 は 0005 実装前は {error, {invalid_key_name, Key, LineNumber}} を返す。0005 実装後は binary_to_atom が無くなり例外が発生しないため、回帰テストではクラッシュしないことだけを確認する（不正 UTF-8 の undoc_ キーは 0005 の設計方針どおりスキップされる）
  - 経路 7 は {error, {invalid_option_value, env_prefix, Value}} を返す
- 各経路の回帰テストが追加されている（経路 3 は「ASCII 文字 + 空白 + 末尾不正バイト」、経路 6 は ASCII 256 文字のキー、経路 7 は不正 UTF-8 の env_prefix を含める）
- unknown_keys / undoc_kv_list の返り値形式の変更に伴い、kvconf の unknown_keys_test / undoc_kv_list_test が更新されている

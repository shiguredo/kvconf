# validate_http_uri が port の範囲を検証しないのを修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-http-uri-port-range
- Polished: 2026-07-31

## 目的

#kvc_http_uri のバリデーションが port の範囲を検証せず、65535 超の port を受理してしまう。正しく検証する。

## 現状

- kvconf_validate の validate_http_uri は scheme / host / path の存在のみ検証し、port を検証しない
- 実測 (OTP 29.0): "https://example.com:99999" が {ok, ...} で受理される（uri_string:parse は port を integer で返す。0 や 65535 も同様に受理される）
- 空 port（"https://example.com:"）は uri_string:parse が port => undefined を返し、現行は {ok, ...} で受理される
- 同一ライブラリの #kvc_port_number は validate_port_number で 0 から 65535 を検証しており、#kvc_http_uri だけ範囲検証がない
- validate_http_uri は 0001（クラッシュ経路のエラー返却化）の経路 2 / 経路 5 と修正箇所が重なる。本 issue は port の範囲検証のみを対象とし、例外のエラー返却化は 0001 に委ねる

## 設計方針

- uri_string:parse の結果に port が含まれ、かつ整数の場合に validate_port_number を再利用して 0 から 65535 の範囲を検証する（validate_port_number は検証にのみ使い、結果は元の URI を {ok, Value} で返す）
- port が undefined（空 port）の場合は現行どおり受理する
- 負の port や非整数の port（"https://example.com:-1" 等）は uri_string:parse が {error, invalid_uri, ...} を返すため、validate_http_uri の catch-all で invalid_value になる（変更不要）
- #kvc_port_number と一貫して port 0 は許容する（0 を拒否する場合は #kvc_port_number との非対称を許容する理由が必要になるため）

## 完了条件

- port が 65535 超（例: 99999）の場合に invalid_value になる
- port 0 / port 5000 / port 65535 / port 省略 / 空 port は ok のまま受理される
- 回帰テストが validate_http_uri_test に追加されている（port 0 / 65535 / 65536 / 99999 / 空 port を含む）

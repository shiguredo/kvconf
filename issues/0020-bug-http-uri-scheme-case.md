# validate_http_uri が scheme の大文字小文字を区別し HTTP:// のような URL を拒否する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-http-uri-scheme-case
- Polished: YYYY-MM-DD

## 目的

`#kvc_http_uri` の検証が scheme を完全一致で比較しており、`HTTP://example.com` のような URL を拒否する。URI の scheme は大文字小文字を区別しないため、受理するように修正する。

## 現状

- `kvconf_validate` の `validate_http_uri/1` は `Scheme =:= <<"https">> orelse Scheme =:= <<"http">>` で判定する
- 実測: `HTTP://example.com` → `{error, {invalid_value, ...}}` (`uri_string:parse/1` は scheme を小文字化せず `<<"HTTP">>` を返す)
- `refs/` に一次資料はないが、RFC 3986 では scheme は大文字小文字を区別しない
- `issues/0007` は同じ関数の port の範囲検証で、対象が重ならない

## 設計方針

- scheme を `string:lowercase/1` してから比較する (`http` / `https` の大文字小文字を問わず受理する)
- 返す値は元の URL のままにする (現行どおり)

## 完了条件

- `HTTP://example.com` / `Https://example.com` が受理される
- 既存の受理・拒否のケース (小文字 scheme の受理、`telnet:` や host なしの拒否) が変わらない
- 回帰テストが `validate_http_uri_test` に追加されている

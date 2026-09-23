# validate_http_uri が scheme の大文字小文字を区別し HTTP:// のような URL を拒否する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-http-uri-scheme-case
- Polished: 2026-09-23

## 目的

`#kvc_http_uri` の検証が scheme を完全一致で比較しており、`HTTP://example.com` のような URL を拒否する。URI の scheme は大文字小文字を区別しないため、受理するように修正する。

## 現状

- `kvconf_validate` の `validate_http_uri/1` は `Scheme =:= <<"https">> orelse Scheme =:= <<"http">>` で判定する
- 実測: `validate_http_uri(<<"HTTP://example.com">>)` は `invalid_value` を返し、`initialize/2` 経由では `{error, {invalid_value, <<"http_uri = HTTP://example.com">>, 1}}` になる (`uri_string:parse/1` は scheme を小文字化せず `<<"HTTP">>` を返す)
- 一次資料 (RFC 3986) はリポジトリ内にない (refs/ ディレクトリも存在しない) が、RFC 3986 Section 3.1 では scheme は case-insensitive であり、実装は大文字を小文字と等価に受理すべきとされている ("An implementation should accept uppercase letters as equivalent to lowercase in scheme names (e.g., allow "HTTP" as well as "http") for the sake of robustness")。Section 6.2.2.1 も scheme は小文字に正規化すべきとしている
- `issues/0007` は同じ `validate_http_uri/1` の port 範囲検証、`issues/0001` は同関数の例外捕捉 (経路 5) を担当する。本 issue は scheme 比較のみを対象とする。0007 の port 検証 (`validate_port_number` の再利用) と 0001 の try-catch が実装済みの場合は、それらを保持したまま scheme 比較のみを変更する (変更するのは `Scheme =:= <<"https">> orelse Scheme =:= <<"http">>` の部分)

## 設計方針

- scheme が binary の場合に `string:lowercase/1` してから `<<"http">>` / `<<"https">>` と比較する。`uri_string:parse/1` は list 入力だと scheme も list で返すが、現行も list 入力の URL は scheme 比較で落ちて `invalid_value` になるため list 入力の挙動は変えない
- 返す値は元の URL のままにする (現行どおり。scheme を小文字化した値を返さない)

## 完了条件

- `validate_http_uri(<<"HTTP://example.com">>)` が `{ok, <<"HTTP://example.com">>}` を返す (`Https://example.com` / `Http://example.com` / `HTTPS://example.com` も同様に受理し、値は入力どおり保持される)
- 既存の受理・拒否のケース (小文字 scheme の受理、`telnet:` や host なしの拒否) が変わらない
- 回帰テストが `validate_http_uri_test` に追加されている
- 従来拒否していた大文字 scheme の URL を受理するユーザーに見える挙動変更を伴うバグ修正のため、CHANGES.md の develop セクションに [FIX] として追記する

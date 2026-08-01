# kvconf.app.src に public_key を宣言する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-declare-public-key-app
- Polished: 2026-07-31

## 目的

kvconf_pkix が public_key を使用しているのに kvconf.app.src の applications に宣言されておらず、kvconf を deps に含むアプリの relx リリースに public_key が収集されず、pkix バリデーション実行時に undef 例外になる。依存を正しく宣言する。

## 現状

- src/kvconf.app.src の applications は [kernel, stdlib] のみ
- kvconf_pkix の validate_pkix_fullchain_pem_file / validate_pkix_cert_pem_file は public_key:pem_decode と public_key:pkix_decode_cert を、validate_pkix_privkey_pem_file は public_key:pem_decode を使用している
- relx / rebar3 のリリースは .app の applications 依存グラフからアプリを収集する（public_key の宣言により、依存の asn1 / crypto も再帰的に収集される）
- 開発時（rebar3 shell 等）は全 OTP ebin がコードパスに入るため検出されない
- kvconf はライブラリとして他アプリの deps に組み込まれるため、問題が顕在化するのは kvconf を deps に含むアプリの relx リリースである。依存宣言が無いと dialyzer の PLT にも public_key が含まれない

## 設計方針

- applications を [kernel, stdlib, public_key] に変更する（asn1 / crypto は public_key のアプリ依存として relx が自動収集するため直接宣言しない）

## 完了条件

- src/kvconf.app.src の applications に public_key が含まれ、rebar3 compile 後の _build/default/lib/kvconf/ebin/kvconf.app に反映されている

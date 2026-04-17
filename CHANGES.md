# 変更履歴

- CHANGE
  - 下位互換のない変更
- ADD
  - 下位互換がある追加
- UPDATE
  - 下位互換がある変更
- FIX
  - バグ修正

## develop

### misc

- [CHANGE] GitHub Actions のコンテナイメージを OTP 28.4.2 / OpenSSL 3.6.2 / Ubuntu 26.04 に上げる
  - @voluntas
- [CHANGE] Slack 通知を rtCamp/action-slack-notify から shiguredo/github-actions/.github/actions/slack-notify に切り替える
  - 通知を別ジョブに分離し `notify_mode` のデフォルト (`failure_and_fixed`) を利用する
  - Fixed 判定のため `actions: read` / `contents: read` 権限と `concurrency` 設定を追加する
  - @voluntas

## 2025.1.0

**リリース日**: 2025-10-08

- [ADD] kvconf:initialize/3 を追加し、環境変数による設定の上書きをサポート
  - Options マップでプレフィックスを指定可能
    - `env_prefix => <<"SPAM">>`
  - `env_prefix` が指定されている場合は環境変数による設定項目の上書きが有効になる
  - `env_prefix` が `<<"SPAM">>` の場合、設定項目 `abc_efg` は環境変数 `SPAM_ABC_EFG` で上書できる
  - @voluntas
- [UPDATE] rebar3 を 3.25.1 に上げる
  - @voluntas

### misc

- [CHANGE] GitHub Actions のコンテナイメージを OTP を 28.1 / OpenSSL 3.6.0 に上げる
  - @voluntas
- [CHANGE] rebar3 の minimum_otp_vsn を 28.1 にする
  - @voluntas
- [CHANGE] GitHub Actions の docker image を ghcr.io に変更する
  - @voluntas
- [ADD] [ELP](https://github.com/whatsapp/erlang-language-platform) 向けの [eqwalizer_support](https://github.com/whatsapp/eqwalizer) を deps に追加する
  - 一時的にコメントアウト
  - @voluntas

## 2024.3.1

- [FIX] `#kvc_http_uri{}` のバリデーションを修正する
  - @voluntas

## 2024.3.0

- [UPDATE] GitHub Actions の docker の OTP を 27.1.2 に上げる
  - @voluntas
- [UPDATE] rebar3 を 3.24.0 に上げる
  - @voluntas
- [ADD] `#kvc_list_atom{}` に candidates を追加する
  - @voluntas

## 2024.2.1

- [ADD] `#kvc_list_string{}` のデフォルト値もチェックするようにする
  - @voluntas

## 2024.2.0

- [ADD] `#kvc_list_string{}` に lowercase を追加する
  - true にすると lowercase で返す
  - @voluntas

## 2024.1.0

- [ADD] `,` 区切りの文字列リストを読み込める `#kvc_list_string{}` を追加
  - @voluntas
- [UPDATE] rebar3 を 3.23.0 に上げる
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を 27.0 に上げる
  - @voluntas
- [CHANGE] rebar3 の minimum_otp_vsn を 27.0 にする
  - @voluntas

## 2023.3.0

- [ADD] `#kvc_interval` に利用可能な時間の単位を `available_time_units` で設定できるようにする
  - デフォルトも影響受けるので要注意
  - ややこしいが min/max は影響を受けない
  - `available_time_units` はデフォルト `undefined` で全ての単位が指定可能
  - `available_time_units` は `[min, s]` とかで指定できる、この場合単位に min と s しか指定できなくなる
  - @voluntas
- [UPDATE] GitHub Actions の docker イメージを OTP 26.1.2 / OpenSSL 3.1.3 に上げる
  - @voluntas

## 2023.2.0

- [UPDATE] rebar3 を 3.22.1 に上げる
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を 26.1.1 に上げる
  - @voluntas
- [CHANGE] rebar3 の minimum_otp_vsn を 26.1 にする
  - @voluntas

## 2023.1.0

- [ADD] rebar3 efmt を追加する
  - @voluntas
- [ADD] rebar3 lint を追加する
  - @voluntas
- [UPDATE] rebar3 を 3.20.0 に上げる
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を 26.0-rc3 に上げる
  - @voluntas

## 2022.2.0

- [UPDATE] rebar3 を 3.18.0 に上げる
  - @voluntas
- [CHANGE] UndocKvList を {atom(), term()} で返すようにする
  - @voluntas

## 2022.1.0

- [CHANGE] kvconf:initialize/2 の戻りを {ok, UnknownKeys, UndocKvList} にする
  - @voluntas
- [CHANGE] rebar3 の minimum_otp_vsn を 24.2 にする
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を 24.2 に上げる
  - @voluntas
- [ADD] slack 通知を secrets.SLACK_INTERNAL_CHANNEL に変更
  - @voluntas

## 2021.5.3

- [FIX] EC PARAMETERS と EC PRIVATE KEY どちらの順番でもよくする

## 2021.5.2

- [FIX] EC PARAMETERS に対応する
- [FIX] EC PRIVATE KEY に対応する

## 2021.5.1

- [FIX] app.src タイポ修正

## 2021.5.0

- [UPDATE] rebar3 を 3.17.0 に上げる
  - @voluntas
- [UPDATE] GitHub Actions の OTP を 24.1.5 に上げる
  - @voluntas
- [CHANGE] 最小 OTP を 24.1 にする
  - @voluntas
- [ADD] rebar3 hex plugin を追加

## 2021.4.1

- [FIX] pkix 関連 validator の戻り値を修正する

## 2021.4

- [ADD] #kvc_pkix_fullchain_pem_file{} を追加
  - @voluntas
- [ADD] #kvc_pkix_privkey_pem_file{} を追加
  - @voluntas
- [ADD] #kvc_pkix_cert_pem_file{} を追加
  - @voluntas

## 2021.3.1

- [FIX] dialyzer 指摘修正漏れ
  - @voluntas

## 2021.3

- [CHANGE] 最小 OTP を 24.0 にする
  - @voluntas
- [ADD] #kvc_list_atom{} を追加
  - atom の list を k = v1, v2, v3 のように指定できるようにする
  - @voluntas
- [UPDATE] GitHub Actions の OTP を 24.0.4 に上げる
  - @voluntas
- [UPDATE] rebar3 を 3.16.1 に上げる
  - @voluntas

## 2021.2

- [UPDATE] GitHub Actions の OTP を 24.0 に上げる
  - @voluntas
- [UPDATE] rebar3 を 3.15.1 に上げる
  - @voluntas

## 2021.1

- [CHANGE] 最小 OTP を 23.2 にする
  - @voluntas
- [ADD] 戻り値に見知らぬキーのリスト (binary) を戻すようにする
  - @voluntas

## 2020.11

- [ADD] `#kvc_interval{}` の `max` に `infinity` を指定可能にする
  - @voluntas

## 2020.10

- [CHANGE] `#kvc_interval{}` の単位を指定する場合は間にスペースをいれる `10 s`
  - `数値と単位を分割するために空白（space）を用いる`
    - <https://ja.wikipedia.org/wiki/%E5%9B%BD%E9%9A%9B%E5%8D%98%E4%BD%8D%E7%B3%BB>
  - @voluntas

## 2020.9.1

- [FIX] `#kvc_interval{}` 利用時の戻り値を {ok, non_neg_integer()} に修正する
  - @voluntas

## 2020.9

- [ADD] `#kvc_interval{}` のデフォルト対応を追加
  - `{100, s}` や `{100, min}` といったように指定可能
  - @voluntas

## 2020.8

- [ADD] `#kvc_interval{}` を追加
  - `key = 100s` や `key = 100min` といった値を指定可能にする
  - erlang 側では out_unit で好きな単位に変更可能
  - @voluntas

## 2020.7

- [UPDATE] デフォルトの値も型チェックするようにする
  - デフォルトには **期待値** を入れるのでデフォルトの値専用のチェックを追加している
  - @voluntas

## 2020.6

- [CHANGE] ipv4_address_and_port_number を削除
  - @voluntas
- [CHANGE] 定義を全て record 化する
  - `#kvc{}` を利用する
  - デフォルトを定義しない場合はかならず undefined になる
  - @voluntas

## 2020.5

- [ADD] atom に変換付きの設定記法を追加する
  - 型の書き方例 `{foo, {atom, [{<<"A4">>, a4}, {<<"B4">>, b4}]}, optional, a4}`
  - 設定を `foo = B4` とすると get_value では `b4` が返ってくる
  - @shino

## 2020.4

- [ADD] list_ipv4_address 型を追加する
  - 設定の書き方 `list_ipv4 = 192.0.2.1, 192.0.2.3`
  - 型の書き方例 `{list_ipv4, list_ipv4_address, options, []}`
  - カンマ区切り、スペースで間を空けても良い
  - @voluntas
- [ADD] list_ipv6_address 型を追加する
  - 設定の書き方 `list_ipv6 = 2001:0DB8::1, 2001:0DB8::2`
  - 型の書き方例 `{list_ipv6, list_ipv6_address, options, []}`
  - カンマ区切り、スペースで間を空けても良い
  - @voluntas

## 2020.3

- [ADD] float 型を追加する
  - 型の書き方例 `{float_foo, {float, -10, 10}, required}`
  - @voluntas

## 2020.2.1

- [FIX] integer 指定時 value に `<<>>` が渡されたときのエラー処理が正しくないのを修正する
  - @Hexa @voluntas

## 2020.2

- [ADD] atom 型を追加する
  - 型の書き方例 `{mode, [auto, manual]}`
  - @shino

## 2020.1

**YYYY.RELEASE[.FIX] にリリース番号を変更する**

- [FIX] uri_string:parse/1 を利用するようにする
  - @voluntas

## 1.0.1

- [FIX] invalid_value のパターンマッチが失敗する問題を修正する
  - @shino

## 1.0.0

最初のリリース

# 変更履歴

- UPDATE
    - 下位互換がある変更
- ADD
    - 下位互換がある追加
- CHANGE
    - 下位互換のない変更
- FIX
    - バグ修正


## develop

- [CHANGE] 最小 OTP を 24.0 にする
    - @voluntas
- [UPDATE] rebar3 を 3.14.4 に上げる
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

- [CHANGE] `#kvc_interval{}` の単位を指定する場合は間にスペースをいれる ``10 s``
    - ``数値と単位を分割するために空白（space）を用いる``
        - https://ja.wikipedia.org/wiki/%E5%9B%BD%E9%9A%9B%E5%8D%98%E4%BD%8D%E7%B3%BB
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

- [FIX] integer 指定時 value に `<<>>`  が渡されたときのエラー処理が正しくないのを修正する
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

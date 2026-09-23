# PropEr による PBT を導入する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/add-proper-pbt
- Polished: 2026-09-23

## 目的

shiguredo-erlang スキルは PBT を導入できるなら PropEr で導入することを求めているが、kvconf は全テストが EUnit で PBT がない。往復性や境界の性質を持つロジックに PBT を導入する。

## 現状

- `rebar.config` に `rebar3_proper` / `proper` への依存がない (`rg proper rebar.config` で 0 件)。`Makefile` の `test` は eunit と cover のみで、`.github/workflows/ci.yml` の実行行は `make compile dialyzer test` のみ
- 時雨堂の他リポジトリ (jsone / base32_clockwork) は `rebar3_proper` と `proper` を test プロファイルに導入し、`Makefile` の `proper` ターゲットと CI の実行行まで組み込んでいる (jsone は `proper: compile` と `make compile dialyzer test proper`、base32_clockwork は `ci: compile dialyzer test proper`)。swidden は依存のみで実行系への組み込みがない
- 性質の対象になる関数は非公開である。`kvconf` の export は `initialize/2,3` / `set_value/2` / `unset_value/1` / `get_value/1` のみ、`kvconf_validate` の export は `validate/3` のみで、`parse/1` / `parse_kv_line/4` / `validate_integer/3` / `validate_float/3` は test の `prop_*` モジュールから呼べない。性質は公開 API に写像して書く
- `include/kvconf.hrl` は `-include_lib("eunit/include/eunit.hrl")` を含み、`proper/include/proper.hrl` の `LET` マクロと衝突するため、`#kvc{}` を組む `prop_*` モジュールはこの include の削除 (issues/0022) の後に着手する
- 性質が明確な候補:
  - `initialize/2,3` と `get_value/1` の往復性 (特殊文字を含まない `key = value` を読み込ませ、`get_value/1` が trim 済みの値と一致する)
  - `kvconf_validate:validate/3` に `#kvc_integer{min, max}` / `#kvc_float{min, max}` を渡した境界の受理・拒否
  - `validate_interval/2` (単位混在時の min / max の境界。issues/0002 と 0017 の修正後。本 issue の対象外)
  - `unknown_keys/2` / `undoc_kv_list/2` (順序・重複の不変条件。issues/0012 / 0018 / 0022 の修正後。本 issue の対象外)

## 設計方針

- test プロファイルに `{plugins, [rebar3_proper]}` と `{deps, [{proper, {git, "https://github.com/proper-testing/proper", {branch, "master"}}}]}` を追加する (兄弟リポジトリと同じ形。test プロファイル専用の依存は rebar.lock に入らないため更新は不要)
- PBT は `test/prop_kvconf.erl` (モジュール名 `prop_kvconf`) に置く。rebar3_proper は `test/` 直下で basename が `prop_` で始まるファイルだけを発見し、export された 0 引数の `prop_*` 関数を実行するため、ファイル名と配置を固定する
- prop モジュールは `-include_lib("proper/include/proper.hrl").` のみを include し、`eunit.hrl` は include しない (`LET` マクロの衝突を避ける)
- 生成器が用意するもの: 重複しない key (`=` / 改行を含まず、空でなく、前後に空白がなく、`#` で始まらない。`parse_lines/3` は行頭の `#` 行をスキップし、`parse_kv_line/4` は key と value を trim するため、これらを含む key では往復性が成立しない)、`=` / 改行を含まない value、`key = value` 行をつないだ設定バイナリ、`#kvc{key = Key, type = #kvc_string{}, required = true}` の KvcList
- 性質は 2 本から始める。境界の固定値ケースは issues/0014 が EUnit で担当するため、本 issue は任意の min / max に対する性質のみを追加する
- `Makefile` に `proper: compile` ターゲット (`@./rebar3 as test proper`) を追加して `.PHONY` と `all` に加え、`.github/workflows/ci.yml` の実行行を `make compile dialyzer test proper` にする (jsone と同じ形)

## 完了条件

- `test/prop_kvconf.erl` に export された 0 引数の `prop_*` 性質が追加されている
- `rebar3 as test proper` が性質を実行して通る (性質が 0 本でも終了コード 0 になるため、`N/N properties passed` の件数で実行されたことを確認する)
- 往復性の性質: 生成した key / value を `key = value` 行にした設定バイナリを `initialize/2,3` に渡し、`KvcList` に同じ key の `#kvc{type = #kvc_string{}, required = true}` を入れたとき、`{ok, [], []}` になり `get_value/1` が `string:trim/1` した value と一致する
- 境界の性質: 任意の `min =< max` の `#kvc_integer{min, max}` / `#kvc_float{min, max}` と値から設定バイナリを作り、`initialize/2,3` が範囲内なら `{ok, [], []}`、範囲外なら `{error, {invalid_value, ...}}` を返す (`max = infinity` は常に範囲内)
- `make test` の EUnit が現行どおり通る (PBT は `test/prop_kvconf.erl` に分離し、`-ifdef(TEST)` ブロック内の EUnit に混ぜない)
- `Makefile` の `proper` ターゲットと CI の `make ... proper` が追加されている
- CHANGES.md の develop の `### misc` に `- [ADD] proper を test 用の依存に追加し、PBT を導入する` と `  - @voluntas` を追記する

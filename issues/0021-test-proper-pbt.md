# PropEr による PBT を導入する

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/add-proper-pbt
- Polished: YYYY-MM-DD

## 目的

shiguredo-erlang スキルは PBT を導入できるなら PropEr で導入することを求めているが、kvconf は全テストが EUnit で PBT がない。往復性や境界の性質を持つロジックに PBT を導入する。

## 現状

- `rebar.config` に `rebar3_proper` / `proper` への依存がない (`rg proper` で 0 件)
- 時雨堂の他リポジトリ (jsone / base32_clockwork / swidden) は `rebar3_proper` と `proper` を導入済み
- 性質が明確な対象:
  - `kvconf` の `parse/1` と `parse_kv_line/4` (特殊文字を含まない `key = value` の読み込み値が `get_value/1` と一致する往復性)
  - `kvconf_validate` の `validate_integer/3` / `validate_float/3` (境界ちょうどの値の受理・拒否)
  - `validate_interval/2` (単位混在時の min / max の境界。`issues/0002` の修正後)
  - `unknown_keys/2` / `undoc_kv_list/2` (順序・重複の不変条件。`issues/0018` の修正後)

## 設計方針

- test プロファイルに `rebar3_proper` と `proper` を追加し、`rebar3 as test proper` で実行する
- 代表的な性質を 1〜2 本から始める (parse の往復性、`validate_integer` / `validate_float` の境界)
- Makefile / CI への組み込みは、性質が増えた段階で別途判断する

## 完了条件

- `rebar3 as test proper` が通る
- parse の往復性と `validate_integer` / `validate_float` の境界の性質テストが追加されている
- `make test` の EUnit が現行どおり通る (PBT と EUnit でモジュール名・配置が衝突しない)

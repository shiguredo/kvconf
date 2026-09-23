# #kvc_interval の単位とフィールドの型検証がなくクラッシュと誤変換が起きる

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-interval-type-validation
- Polished: 2026-09-23

## 目的

`#kvc_interval` の `min` / `max` / `available_time_units` / `out_time_unit` に型注釈どおりでない値を書くと、`initialize` がクラッシュしたり、無言で別の単位に変換された値が保存されたりする。実行時に検証してエラーとして返す。

## 現状

- `kvconf` の `in_time_unit()` は `ms | s | min | h`、`out_time_unit()` は `second | millisecond | microsecond` だが、`kvconf_validate` の `validate_interval/2` は入力値の単位しか検証しない
- 実測 (`initialize` 経由で再現):
  - `min = {10, sec}` / `max = {1, sec}` → `error:function_clause` (`time_unit/1` に節がない)
  - `available_time_units = ms` / `= sec` (非リスト) → 値が正常でも `error:function_clause` (`validate_available_time_unit/2` に catch-all がない)
  - `out_time_unit = sec` + `default = {10, s}` → `error:badarg` (tuple 経路は `catch error:badarg` の外。binary 経路は `invalid_value` に隠蔽される)
  - `out_time_unit = native` / `= nanosecond` → `{ok, [], []}` で受理され、`1 ms` が `1000000` として保存される (型にない単位が無言で通る)
- `issues/0001` は `min` / `max` / `out_time_unit` の `undefined` を扱い、`available_time_units` の型不正は対象外としている。本 issue は単位と型の検証に絞る

## 設計方針

- `validate_interval/2` の入口で `min` / `max` / `out_time_unit` / `available_time_units` の型と単位を検証し、不正なら `invalid_value` を返す
  - `min` は `{non_neg_integer(), ?IN_TIME_UNIT の要素}` のみ（`infinity` は許容しない。include/kvconf.hrl の型注釈に `infinity` がなく `validate_interval_min/2` に `infinity` 節もないため、許容するとクラッシュが残る）
  - `max` は `{non_neg_integer(), ?IN_TIME_UNIT の要素}` または `infinity`
  - `out_time_unit` は `second | millisecond | microsecond` に限定する
  - `available_time_units` は `undefined` または `?IN_TIME_UNIT` の要素のみの proper list（improper list も `invalid_value`）
- `undefined` は入口検証では素通しし、`issues/0001` の `min` / `max` / `out_time_unit` の `undefined` 判定に委ねる（本 issue の検証規則に `undefined` を含めない）。0001 は `undefined` のみを扱うため、現状の実測 4 ケースは 0001 では解消されず、本 issue の実装が必要になる
- `issues/0001` と同じ関数を触るため、`0001` の実装後に着手するか、`0001` の設計方針と合わせて実装する
- `validate_available_time_unit/2` の `-spec` が存在しないモジュール `kv_conf` を参照している（正しくは `kvconf`。rebar.config の dialyzer 警告 `no_unknown` により現状は検出されない）ため、あわせて修正する

## 完了条件

- 上記の実測ケースがクラッシュせず `invalid_value` になる
- `min = infinity` が `invalid_value` になる（`max = infinity` は従来どおり許容する）
- `out_time_unit = native` / `nanosecond` が拒否される
- `available_time_units` に improper list を指定した場合も `invalid_value` になる
- 正常な `min` / `max` / `out_time_unit` / `available_time_units` の組合せは現行どおり動作する (回帰)
- `validate_available_time_unit/2` の `-spec` の `kv_conf` が `kvconf` に修正されている
- 回帰テストが `validate_interval_test` に追加されている（0002 の単位混在 max 境界と 0014 のエラーパスは対象外。本 issue は実測の型・単位不正ケースと、既存の正常系が引き続き通ることの確認のみを追加する）
- クラッシュが `invalid_value` に変わり `out_time_unit = native` / `nanosecond` の受理が廃止されるユーザーに見える挙動変更を伴うバグ修正のため、CHANGES.md の develop セクションに [FIX] として追記する（README は `out_time_unit` を `second` / `millisecond` / `microsecond` のみと記載しており、型注釈外の値を受け付けないようにする修正のため [FIX] とする）

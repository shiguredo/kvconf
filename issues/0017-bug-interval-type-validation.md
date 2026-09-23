# #kvc_interval の単位とフィールドの型検証がなくクラッシュと誤変換が起きる

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-interval-type-validation
- Polished: YYYY-MM-DD

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
  - `min` / `max` は `{non_neg_integer(), ?IN_TIME_UNIT の要素}` または `infinity`
  - `out_time_unit` は `second | millisecond | microsecond` に限定する
  - `available_time_units` は `undefined` または `?IN_TIME_UNIT` の要素のみのリスト
- `undefined` の扱いは `issues/0001` に委ね、本 issue は単位と型の検証のみを行う
- `issues/0001` と同じ関数を触るため、`0001` の実装後に着手するか、`0001` の設計方針と合わせて実装する

## 完了条件

- 上記の実測ケースがクラッシュせず `invalid_value` になる
- `out_time_unit = native` / `nanosecond` が拒否される
- 正常な `min` / `max` / `out_time_unit` / `available_time_units` の組合せは現行どおり動作する (回帰)
- 回帰テストが `validate_interval_test` に追加されている

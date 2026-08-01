# #kvc_interval の max 境界が単位換算の切り捨てで甘くなるのを修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-interval-max-boundary
- Polished: 2026-07-31

## 目的

#kvc_interval の max 境界チェックが単位換算の整数切り捨てにより、max を超える値を許容して保存してしまう。正しく max 超過を拒否するように修正する。

## 現状

- kvconf_validate の validate_interval_max は値を max の単位に erlang:convert_time_unit/3 で変換してから max と比較する（time_unit による正規化を経て比較単位は us / ms / s のいずれかになる）
- 変換時に整数切り捨てが起きるため、値の単位が max の単位より細かい場合に max 超過値が通る
- 実測: validate_interval(<<"1001 ms">>, #kvc_interval{min = {0, ms}, max = {1, s}, out_time_unit = millisecond}) は {ok, 1001} で受理される。1999 ms も同様に受理され 1999 が保存される（2000 ms は invalid_value）
- min 側（validate_interval_min）は同じ構造だが、値が min 未満なら切り捨て変換後も min 未満にしかならず、値が min 以上なら切り捨て変換後も min 以上にしかならないため誤受理も誤拒否も起きず、min と max で非対称に壊れている
- validate_interval_test に、切り捨てにより max 超過が誤受理される単位混在のケースが無いため検出されていない
- validate_interval_max は 0001（undefined によるクラッシュ経路のエラー返却化）と修正箇所が重なる。0001 は例外のエラー返却化のみを行い、本 issue は境界比較ロジックのみを対象とする

## 設計方針

- max を値の単位に変換して比較する形に変える（現在は値側を max の単位に変換している）。値は常にその単位の整数倍であるため、max 側の変換で起きる切り捨ては比較の正確性に影響しない（ceil での変換は不要。max 側を ceil で変換すると max = {999, ms} / 値 = {1, s} のようなケースで誤受理する）
- min 側は現在の実装が正確であるため修正しない
- max = infinity の場合は現行どおり常に受理する分岐を維持する

## 完了条件

- max = {1, s} のとき 1001 ms 以上が invalid_value になり、1000 ms（境界ちょうど）は ok のまま受理される
- 単位混在の max 境界の回帰テストが validate_interval_test に追加されている
  - 1000 ms / 1001 ms / 1999 ms / 2000 ms vs max = {1, s}（1000 ms は ok、1001 / 1999 / 2000 ms は invalid_value）
  - 1 s vs max = {999, ms}（invalid_value。max 側を ceil で変換する誤実装を検出する）
  - min 側の 999 ms / 1000 ms / 1001 ms vs min = {1, s}（999 ms は invalid_value、1000 / 1001 ms は ok）

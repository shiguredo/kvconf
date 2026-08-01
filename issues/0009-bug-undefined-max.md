# validate_integer / validate_float の max = undefined が無制限許容になるのを修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-undefined-max
- Polished: 2026-07-31

## 目的

#kvc_integer / #kvc_float の max に undefined を指定すると上限なしとして全値が許可される。型注釈（integer() | infinity / float() | infinity）の範囲外の値（undefined 等の atom 一般）が黙って機能するのを修正する。

## 現状

- kvconf_validate の validate_integer / validate_float は max を undefined で受けると、Erlang の項順序（number < atom）により Value =< undefined が常に真になり全値が許可される（max = foo のような他の atom でも同様）
- 実測: validate_integer(999999, 0, undefined) は {ok, 999999} になる（initialize 経由では {ok, [], []}）。#kvc_float も同構造で全許可になる
- min = undefined は undefined =< Value が常に偽になり全拒否。min と max で非対称に壊れている
- #kvc_integer / #kvc_float は max にデフォルト値を持たないため、#kvc_integer{min = 0} のように max を省略すると undefined になり、本修正後は invalid_value になる（上限なしとして機能していた定義は互換性が壊れる）
- #kvc_interval の min / max の undefined は 0001（クラッシュ経路のエラー返却化）の経路 1 の対象のため本 issue では扱わない。validate_integer / validate_float は 0001 の対象外であり重複しない

## 設計方針

- max = undefined は invalid_value にする（0001 の #kvc_interval の undefined 扱いと同じ実行時検証方式。KvcList 定義時検証は kvconf にその機構がなく、0001 の方針「KvcList のレコード定義は静的データ」とも不整合のため不採用）
- validate_integer / validate_float は min / max が number であること（is_number）をガードで検査し、undefined 等の atom を排除する。#kvc_float の min / max に integer を指定する既存利用（smoke_test の #kvc_float{min = -10, max = 10} 等）と、#kvc_integer の min / max に float を指定する既存挙動を維持するため、型注釈どおりの is_float / is_integer 限定はしない
- min 側は既に全拒否で挙動不変だが、max 側のガード追加と対称にするため同じ is_number ガードに含める
- max = infinity は is_number ガードの適用除外で従来どおり許容する

## 完了条件

- max = undefined が全許可にならない（validate_integer(999999, 0, undefined) と validate_float(1.5, 0.0, undefined) が invalid_value）
- max = foo 等の undefined 以外の atom も invalid_value になる（validate_integer(999999, 0, foo)）
- min = undefined や min = foo 等の atom も invalid_value になる（min 側は現行でも成立するが、max 側と同じガードで明示的に扱う）
- max = infinity は従来どおり全許可のまま（回帰）
- #kvc_float の min / max に integer を指定しても従来どおり動作する（既存 smoke_test の回帰）
- default 値の検証経路でも max = undefined が全許可にならない（default 値も validate_type で検証されるため）
- 回帰テストが validate_integer_test に追加され、validate_float_test が新規作成されている（max = infinity / 範囲外のテストは 0014 が担い、undefined / atom のテストは本 issue が担う）

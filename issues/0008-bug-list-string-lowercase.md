# #kvc_list_string の lowercase が UTF-8 を破壊し default に適用されないのを修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-list-string-lowercase
- Polished: 2026-07-31

## 目的

#kvc_list_string の lowercase = true のときの小文字化が latin1 解釈により UTF-8 を破壊し、また default 値には適用されない。両方を修正する。

## 現状

- kvconf_validate の validate_list_string の lowercase 経路は string:to_lower(binary_to_list(Value)) を使用し、バイト列を latin1 と解釈する
- UTF-8 マルチバイト文字のリードバイトが変換され、不正な UTF-8 を生成する。実測: "café"（0xC3 0xA9）の 0xC3 が latin1 の Ã として 0xE3 に変換され、<<99, 97, 102, 227, 169>> という不正な UTF-8 になる
- is_list 節（default チェック）は Lowercase を無視するため、リスト形式の default 値は lowercase されない（ファイル値はされる）
- validate_list_string は 0001（クラッシュ経路のエラー返却化）の対象外であり、不正 UTF-8 入力のクラッシュ保護は本 issue で対応する

## 設計方針

- binary のまま string:lowercase/1 を適用する（binary_to_list を経由しない。binary_to_list 経由だと latin1 解釈が残り UTF-8 破壊が再現する）
- string:lowercase/1 は不正 UTF-8 バイト列で error:{badarg, Binary} タプル形式の例外を投げるため、lowercase 適用は catch error:{badarg, _} で捕捉して invalid_value を返す（catch error:badarg では捕捉できない。0001 の {error, term()} 契約に整合させる。default 経路の要素が不正 UTF-8 の場合も同様）
- lowercase 適用後に従来どおり binary:split + trim_all で要素分割する（順序は現行実装と同一）
- default の is_list 節は lowercase の有無で分岐し、true のときは各要素に string:lowercase を適用したリストを返す（リスト全体に適用すると chardata としてフラット化されるため、要素ごとに適用する）
- default への適用により、既存の大文字入り default は小文字化されて返るようになる（ファイル値と同じ仕様。意図的な挙動変更）

## 完了条件

- UTF-8 を含む値（例: "café"）が lowercase されても壊れない
- 不正 UTF-8 を含む値が lowercase 経路でクラッシュせず invalid_value になる
- リスト形式の default 値にも lowercase が適用される（大文字を含む default 値のケースを含む）
- 回帰テストが validate_list_string_test に追加されている

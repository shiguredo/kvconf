# リスト型バリデータが improper list の default や candidates でクラッシュする

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-list-default-improper
- Polished: YYYY-MM-DD

## 目的

リスト型の default 値や `#kvc_list_atom` の candidates に improper list を書くと `initialize` がクラッシュする。型注釈どおりでない KvcList の記述でクラッシュせず、`invalid_value` を返すようにする。

## 現状

- `is_list/1` は improper list でも true を返すため、`kvconf_validate` の `validate_list_string/2` / `validate_list_ipv4_address/1` / `validate_list_ipv6_address/1` / `validate_list_atom/2` の `is_list` 節が `lists:all/2` に improper list を渡し、末尾で例外になる
- 実測:
  - `#kvc_list_string{default = [<<"a">> | b]}` → `error:function_clause` (`lists:all/2`)
  - `#kvc_list_ipv4_address{default = [{1, 2, 3, 4} | b]}` / `#kvc_list_ipv6_address{default = [{0, 0, 0, 0, 0, 0, 0, 1} | b]}` → 同様
  - `#kvc_list_atom{candidates = [a, b], default = [a | b]}` → 同様
  - `#kvc_list_atom{candidates = [a | b]}` に一致しない値を渡すと `validate_atom/2` の節がなく `error:function_clause` (一致する値なら通る)
- `issues/0001` は default の「要素の型不正」を扱うが、improper list は記載がない
- `issues/0016` は `validate_list_atom` の binary 経路の内部バグで、本 issue とは原因が別

## 設計方針

- 各 `is_list` 節の入口で proper list か確認し、improper list は `invalid_value` を返す (`lists:all/2` に渡す前に弾く)
- `validate_atom/2` / `validate_list_atom/2` は candidates が improper list の場合に節がないため、catch-all 節か candidates の検証で `invalid_value` を返す
- `issues/0001` と同じ関数を触るため、`0001` の実装後に着手するか、`0001` の設計方針と合わせて実装する

## 完了条件

- 上記の improper list の default / candidates でクラッシュせず `invalid_value` になる
- candidates が正常なリストのときの既存挙動が変わらない (回帰)
- 回帰テストが `validate_list_atom_test` / `validate_list_string_test` に追加されている

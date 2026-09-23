# リスト型バリデータと #kvc_atom が improper list の default や candidates でクラッシュする

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-list-default-improper
- Polished: 2026-09-23

## 目的

リスト型の default 値や `#kvc_list_atom` / `#kvc_atom` の candidates に improper list を書くと `initialize` がクラッシュする。型注釈どおりでない KvcList の記述でクラッシュせず、`invalid_value` を返すようにする。

## 現状

- `is_list/1` は improper list でも true を返すため、`kvconf_validate` の `validate_list_string/2` / `validate_list_ipv4_address/1` / `validate_list_ipv6_address/1` / `validate_list_atom/2` の `is_list` 節が `lists:all/2` に improper list を渡し、末尾で例外になる
- 実測 (`#kvc{}` の `type` と `default` で表記する):
  - `#kvc{type = #kvc_list_string{}, default = [<<"a">> | b]}` を `initialize/2` に渡すと `error:function_clause` (`lists:all/2` の内部で発生し、呼び出し元は `validate_list_string/2`)
  - `#kvc{type = #kvc_list_ipv4_address{}, default = [{1, 2, 3, 4} | b]}` / `#kvc{type = #kvc_list_ipv6_address{}, default = [{0, 0, 0, 0, 0, 0, 0, 1} | b]}` も同様
  - `#kvc{type = #kvc_list_atom{candidates = [a, b]}, default = [a | b]}` も同様
- candidates が improper list の場合、例外の発生元は値の形で異なる:
  - `#kvc{type = #kvc_list_atom{candidates = [a | b]}, default = [c]}` (list 値) → `validate_atom/2` の `is_atom(Value)` 節内の `lists:any/2` が improper tail で `error:function_clause` になる (節の欠落ではない)
  - `#kvc{type = #kvc_list_atom{candidates = [a | b]}, default = <<"c">>}` (binary 値) → 候補走査が tail に達し、`validate_atom/2` に非リスト項の tail を受け取る節がないため `error:function_clause` になる (既存の catch-all 節 `validate_atom(Value, [_ | Candidates])` はリスト tail のみを対象にしている)
  - improper tail より前に一致候補がある場合は現状 `{ok, [], []}` になる (実測: `default = [a]` は通る)
- `#kvc_atom` も同じ `validate_atom/2` を共有するため、`candidates = [c | b]` で `default = a` (一致しない) や `candidates = [a | b]` で `default = b` は `error:function_clause` になる
- `default` が non-list の場合は 0001 の経路 2 (default の型不正) の担当で、本 issue の対象外
- `issues/0001` は default の「要素の型不正」を扱い、improper list は本 issue に委譲している。`issues/0008` は同じ `validate_list_string/2` の `is_list` 節を触るが、proper list の判定は本 issue、lowercase 適用と全要素 binary 検査は 0008 が担当する
- `issues/0016` は `validate_list_atom` の binary 経路の内部バグで、本 issue とは原因が別

## 設計方針

- `kvconf_validate` に非公開ヘルパ `is_proper_list/1` (`try length(List) of _ -> true catch error:badarg -> false end`) を追加し、`validate_list_string/2` / `validate_list_ipv4_address/1` / `validate_list_ipv6_address/1` / `validate_list_atom/2` の `is_list` 節の入口で `is_proper_list(Value)` を確認し、improper list は `invalid_value` を返す (`lists:all/2` に渡す前に弾く)。`validate_list_string/2` では 0008 の lowercase 分岐より前に判定を置く
- `validate_atom/2` の入口で `candidates` が proper list でなければ `invalid_value` を返す。末尾に catch-all 節を足す案は list 値の経路 (`is_atom(Value)` 節内の `lists:any/2`) を直せないため採らない
- candidates が improper list の場合は、improper tail より前に一致候補がある場合も含めて `invalid_value` にする (KvcList の記述誤りとして扱う)
- `issues/0001` は improper list を本 issue に委譲しており、本 issue が触る `is_list` 節の proper list 判定は 0001 の変更対象と重ならないため、実装順に依存せず単独で着手できる
- `#kvc_atom` は `validate_atom/2` を共有するため対象に含める

## 完了条件

- validator は improper list の default / candidates で `invalid_value` を返し、`initialize/2` 経由では `{error, {invalid_value, Key, LastLineNumber}}` を返す (クラッシュしない)
- candidates が improper list の場合は、一致候補の有無にかかわらず `invalid_value` になる
- candidates が正常なリストのときの既存挙動が変わらない (回帰)
- 回帰テストが `validate_list_string_test` / `validate_list_ipv4_address_test` / `validate_list_ipv6_address_test` / `validate_list_atom_test` / `validate_atom_test` に追加されている
- クラッシュが `{error, {invalid_value, ...}}` に変わるユーザーに見える挙動変更を伴うバグ修正のため、CHANGES.md の develop セクションに [FIX] として追記する

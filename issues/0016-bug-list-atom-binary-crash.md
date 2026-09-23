# validate_list_atom の binary 経路で不正要素の後ろに有効要素があるとクラッシュする

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-list-atom-binary-crash
- Polished: 2026-09-23

## 目的

`#kvc_list_atom` の binary 検証で、候補に無い要素の後ろに候補にある要素が続くと `initialize` がクラッシュする。`{error, term()}` を返す契約を守る。

## 現状

- `kvconf_validate` の `validate_list_atom/2` の binary 節は `lists:foldl/3` で検証結果を蓄積し、`invalid_value` になった後も `[Atom | Acc]` を積む。そのため Acc が improper list になり、最後の `lists:reverse/1` が例外になる
- 実測 (`candidates = [a, b]`):
  - `la = a,x,b` (不正が中間・有効が後続) → `error:function_clause`
  - `la = x,a` (不正が先頭・有効が後続) → `error:function_clause`
  - `la = a,x` (不正が末尾) → `{error, {invalid_value, <<"la = a,x">>, 1}}` で正常
- 要素の位置でクラッシュの有無が変わるため、設定ファイルの記述ミスがクラッシュとして現れる
- `issues/0001` の 7 経路 (default 値の型不正や他バリデータ) とは別経路 (0001 の経路 2 は default の非 binary 型不正で `validate_list_atom` の catch-all 節が対象。0001 の設計方針は `validate_list_atom` の binary 経路を本 issue が担当と明記している)。`issues/0023` (default / candidates の improper list) とも原因が別で、こちらは内部の蓄積処理のバグ

## 設計方針

- `lists:foldl/3` をやめ、`invalid_value` が確定した時点で残りの要素を検証せず返す再帰にする (`lists:all/2` と変換の組合せでもよいが、どの要素が不正かは問わないためエラー形式は変えない)
- エラー形式 `{error, {invalid_value, Line, LineNumber}}` は現行のままにする

## 完了条件

- 上記の入力が `{error, {invalid_value, Line, LineNumber}}` になりクラッシュしない
- 不正要素が先頭・中間・末尾のいずれでも同じエラーになる
- 回帰テストが `validate_list_atom_test` に追加されている (candidates = `[a, b]` で入力 `~"A,a"` が `invalid_value` になる、大文字の `A` が候補に一致しないケースを含む)
- クラッシュが `{error, {invalid_value, ...}}` に変わるユーザーに見える挙動変更を伴うバグ修正のため、CHANGES.md の develop セクションに [FIX] として追記する

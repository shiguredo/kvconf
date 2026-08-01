# 空キーの設定行（= value）を構文エラーにする

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-empty-key-line
- Polished: 2026-07-31

## 目的

"= value" のような空キーの設定行が黙って受理され、UnknownKeys に <<>> が載る。設定ミス（キー名の書き忘れ）が検出されずに通り過ぎるため、構文エラーとして invalid_line_format を返す。

## 現状

- kvconf の parse_lines の正規表現 ^([^=]*)=(.*)$ が空キーにマッチし、string:trim 後のキーが <<>> のまま設定に追加される
- 実測: initialize([], <<"= value\n">>, #{}) が {ok, [<<>>], []} を返す。"   = value" のような空白のみのキーも同様
- 空キーは unknown_keys の binary_to_atom(<<>>) により空 atom '' を生成する（0005 の atom 生成の修正範囲に関連。本 fix で parse 段階で拒否される）
- 空キー行が 2 回以上出現すると現行は {error, {duplicated_key, <<>>, ...}} になるが、本 fix 後は 1 行目で invalid_line_format になる
- 0001 は parse_lines の string:trim の例外エラー返却化を、0006 はコメント・空白行判定の正規表現を担当する。本 issue は空キー判定のみを対象とする

## 設計方針

- trim 後にキーが空の場合は {error, {invalid_line_format, Line, LineNumber}} を返す（Line は trim 前の生の行。parse_lines の trim 直後に判定を追加する。値行の判定正規表現 ^([^=]*)=(.*)$ は変更しない）
- 値が空の行（"key ="）は対象外。empty_string = → <<>> は smoke test で固定された仕様
- "=" のみの行（キー・値とも空）はキー空として invalid_line_format になる

## 完了条件

- initialize([], <<"= value\n">>, #{}) が {error, {invalid_line_format, <<"= value">>, 1}} を返す
- initialize([], <<"   = value\n">>, #{}) が {error, {invalid_line_format, <<"   = value">>, 1}} を返す（Line は trim 前の生の行）
- "key ="（値が空）は引き続き受理される（smoke test の empty_string = の回帰）
- 回帰テストが kvconf.erl 内の eunit に追加されている

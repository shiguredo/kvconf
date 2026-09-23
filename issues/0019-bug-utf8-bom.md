# UTF-8 BOM 付きの設定ファイルで先頭キーが BOM 込みのキーとして扱われる

- Created: 2026-09-23
- Completed: YYYY-MM-DD
- Branch: feature/fix-utf8-bom
- Polished: YYYY-MM-DD

## 目的

UTF-8 BOM 付きで保存された設定ファイルを読み込むと、先頭キーに BOM が混ざった別のキーとして扱われ、未知キーとして無言で通る。BOM の扱いを定めて修正する。

## 現状

- `kvconf` の `parse_lines/3` は BOM を除去せず、先頭行を通常の `key=value` 行として処理する
- 実測: `initialize([], <<16#EF, 16#BB, 16#BF, "key = value\n">>)` → `{ok, [<<"ï»¿key">>], []}` (`key` ではなく BOM 込みのキーが未知キーとして返る。エラーにはならない)
- BOM の扱いを定めた記述は README / CHANGES / issues にない
- `issues/0006` (CRLF・タブ) と同じ `parse_lines/3` を触る

## 設計方針

- 先頭の BOM を除去して受理する方式を第一候補とする (BOM 付きで保存するエディタがあり、拒否すると既存の設定ファイルが読めなくなるため)
- BOM を拒否 (`invalid_line_format`) する方式を採る場合は、その理由を明記する

## 完了条件

- BOM 付きの設定ファイルで先頭キーが `key` として扱われる (除去方式) か、`invalid_line_format` で拒否される (拒否方式)
- BOM 以外の行 (2 行目以降) の挙動が変わらない
- 回帰テストが `kvconf.erl` の eunit に追加されている

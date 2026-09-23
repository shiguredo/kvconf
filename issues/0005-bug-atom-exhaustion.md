# unknown_keys / undoc_kv_list の binary_to_atom による atom 無制限生成を止める

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-atom-exhaustion
- Polished: 2026-09-23

## 目的

設定ファイルのキーを binary_to_atom で無制限に atom 化しており、設定再読込で一意な未知キーが増えると atom テーブル枯渇により VM がクラッシュしうる。atom を生成しない形に修正する。

## 現状

- kvconf の unknown_keys は maps:keys(Configurations) の全キーを binary_to_atom で atom 化してから KvcList を検索する
- undoc_kv_list も undoc_ 接頭辞キーを binary_to_atom で atom 化する
- atom は GC されないため、設定再読込のたびに一意な未知キー分だけ atom テーブルが肥大化する
- kvconf_validate の validate_interval は binary_to_existing_atom を try-catch と組み合わせて既存 atom にのみ変換しており atom を新規生成しない。一方 unknown_keys / undoc_kv_list は binary_to_atom で新規生成しており、ライブラリ内で方針が揃っていない
- 0001（クラッシュ経路のエラー返却化）の経路 6 は unknown_keys / undoc_kv_list の binary_to_atom による不正 UTF-8 キーの badarg と、256 文字以上のキーの system_limit を扱っており、対象箇所が重なる。本 issue の修正で未知キー検出がバイナリ比較に、undoc_ キーの atom 化が binary_to_existing_atom になるため経路 6 は解消される（長いキーの回帰テストは 0001 が担当する）。返り値形式の変更（unknown_keys / undoc_kv_list の {ok, ...} ?= 化）は 0001 が担当し、0012 も同じ分担を前提にしている。本 issue は返り値の外側の形式には触れず、本体の atom 生成停止のみを行う
- 0012（persistent_term の stale 値）は undoc_kv_list の値の取得元（persistent_term / 設定ファイル）を扱う。本 issue は atom 生成の停止のみを対象とし、値の取得ロジックは 0012 の担当領域とする

## 設計方針

- unknown_keys は KvcList のキーを atom_to_binary(Key, utf8) で binary 化したリストと Configurations のキー（binary）を比較する。未知キー検出が例外に依存せず、KvcList のキーはソースリテラルの atom であるため atom_to_binary(Key, utf8) は成功する
- undoc_kv_list は Configurations の undoc_ キーを binary_to_existing_atom で既存 atom にのみ変換し、badarg（atom 未存在・不正 UTF-8）は try-catch で捕捉してスキップする（不正 UTF-8 の undoc_ キーはエラーにならず黙ってスキップされる。0001 は unknown_keys / undoc_kv_list の両方の binary_to_atom を try-catch で保護するが、本 issue の実装後はこの保護は発火せず、undoc_kv_list ではスキップに置き換わる）。persistent_term に値がある undoc_ キーの atom は set_value 時に生成済みであるため binary_to_existing_atom は成功し、atom は新規生成されない。リストの要素は {atom(), term()} のタプルのまま維持する（返り値の外側の {ok, ...} 化は 0001 が担当する）。KvcList に無い undoc_ キーを返すかどうかは 0012（値の取得元の変更）で確定する（0012 の方針は「Configurations に存在する undoc_ キーのみを返す」）
- 本 issue は 0001 の実装順に依存しない。0001 が追加した経路 6 の回帰テストが本 issue の実装で不成立になる場合は、本 issue 側で期待値を更新する（不正 UTF-8 のキーは unknown_keys に載り、undoc_ キーはスキップされる）
- validate_interval の binary_to_existing_atom は変更対象外（閉じた集合の単位検証に try-catch と組み合わせて使う正しい実装であり、本 issue の修正対象は unknown_keys / undoc_kv_list のみ）

## 完了条件

- 毎回異なる未知キーを含む設定で initialize を繰り返しても、erlang:system_info(atom_count) が増えない（回帰テストは initialize 実行前後の diff で検証し、テスト内で binary_to_atom 等による atom 新規生成を行わない）。計測は次を満たすこと
  - 計測前に initialize を 1 回実行して遅延モジュールロードを済ませてから atom_count を測る（初回呼び出しは kvconf_validate 等のロードで atom_count が増えるため）
  - 未知キーだけでなく undoc_ 接頭辞の一意キーも含める（現行コードは undoc_kv_list 側でも 1 キーあたり 1 atom を生成するため）
- unknown_keys がバイナリ比較で未知キーを検出すること。unknown_keys_test / undoc_kv_list_test が通ること（返却順の決定性は 0018 が担当するため、本 issue では既存テストの期待順序を変更しない。0018 が先に実装されている場合はソート後の要素比較になっている前提で通す）。undoc_kv_list が atom を新規生成しないこと（返すキー集合と undoc_kv_list_test の扱いは 0012 の設計方針に従う）
- 回帰テストが追加されている

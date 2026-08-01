# kvconf_pkix のテストを追加する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/add-pkix-tests
- Polished: 2026-07-31

## 目的

0001（クラッシュ経路のエラー返却化）と 0003（privkey の検証強化）の実装に合わせ、kvconf_pkix の 3 つの検証関数のテスト一式（エラー系を含む）を追加する。バリデーションの誤動作（証明書を秘密鍵として受理する等）が検出されないままになっているのを防ぐ。

## 現状

- test ディレクトリに PEM フィクスチャがなく、validate_pkix_fullchain_pem_file / validate_pkix_privkey_pem_file / validate_pkix_cert_pem_file が一度も実行されていない
- カバレッジレポート（cover）上も kvconf_pkix は 0%（実測: kvconf 94% / kvconf_validate 93% / 全体 86%）
- 0003 の完了条件は「本 issue は正常系とタイプ検証の回帰テストを追加し、エラー系のテストは 0013 が担う」としており、本 issue は 0003 が列挙したエラー系（混在ファイル / 1 エントリの 'EcpkParameters' のみ / 'EcpkParameters' + 'ECPrivateKey' 以外の組み合わせ / 3 エントリ以上 / 壊れた DER を含む）を検証する

## 設計方針

- public_key:pkix_test_data/1 でテスト実行時に証明書チェーンを生成し、一時ファイルに書き出して検証する（秘密鍵をリポジトリにコミットしない）。pkix_test_data/1 は #{server_config, client_config} 形式を直接返し（{ok, ...} で包まれない）、各値は [{cert, DER}, {key, {Asn1Type, DER}}, {cacerts, [DER, ...]}] の proplist 形式である。デフォルトの鍵は 'ECPrivateKey'。fullchain は cert と cacerts を連結して組み立てる
- 一時ファイルは eunit の setup/teardown で一時ディレクトリを生成・削除して管理する
- エラー系はバリデータ単体でテストし、期待値は error アトムに統一する（{error, {error, ...}} の形式になるのは initialize 経由のみであり、0001 の回帰テストの担当とする）
- エラー系の各ケースは 3 つの検証関数すべてに適用する（3 関数は同一構造の分岐を持ち、1 関数だけに適用するとカバレッジ 80% に届かない）。privkey 特有のケース（'EcpkParameters' のみ等）は privkey のみに適用する
- エラー系のフィクスチャは全てテスト実行時に public_key API で生成する
  - 暗号化 PEM は der_decode で ASN.1 entity に戻してから pem_entry_encode/3（暗号化オプション付き）で生成する
  - SubjectPublicKeyInfo は公開鍵レコード（#'RSAPublicKey'{} 等）を private 鍵レコードから組み立てて pem_entry_encode('SubjectPublicKeyInfo', ...) で生成する
  - 存在しないファイルパス / 空ファイル / 不正 base64 の PEM / 3 エントリ以上のファイルは binary を直接組み立てて生成する
- エラー系のケース:
  - 存在しないファイルパス（3 関数すべて）
  - 空ファイル（pem_decode が [] を返す分岐。3 関数すべて）
  - 不正 base64 の PEM（3 関数すべて。0001 の initialize 経路テストとは別に、バリデータ単体で error を返すことを確認する）
  - 暗号化 PEM（3 関数すべて）
  - 'Certificate' タイプだが DER が壊れたエントリ（pkix_decode_cert の catch 節。fullchain と cert の両方）
  - 証明書を秘密鍵に渡した場合（privkey。0003 の修正後の挙動を検証する）
  - SubjectPublicKeyInfo のみのファイル（privkey。0003 の修正後の挙動を検証する）
  - 秘密鍵 + 証明書の 2 エントリ混在ファイル（privkey。0003 の修正後の挙動を検証する）
  - 1 エントリの 'EcpkParameters' のみのファイル（privkey。0003 の修正後の挙動を検証する）
  - 'EcpkParameters' + 'ECPrivateKey' 以外の組み合わせの 2 エントリファイル（privkey。0003 の修正後の挙動を検証する。秘密鍵 + 証明書の混在はこの組み合わせ不正の一部としてカバーされる）
  - 3 エントリ以上のファイル（privkey）
  - 壊れた DER のファイル（PRIVATE KEY ラベルにゴミ base64 を含むファイル。privkey。0003 の修正後の挙動を検証する）
- 本 issue は 0001（クラッシュ経路のエラー返却化）と 0003（privkey の検証強化）の実装後に着手する。privkey のエラー系の期待値は 0003 実装後の挙動を基準にする

## 完了条件

- kvconf_pkix モジュールの行カバレッジが 80% 以上になる（make test 実行後の _build/test/cover/index.html で確認する。kvconf_pkix は現在 0% であり、上記の正常系・エラー系を 3 関数に適用すれば 80% を超える）
- pkix_test_data/1 で生成した正常な証明書チェーン / 秘密鍵 / 証明書がそれぞれ {ok, ...} になることを検証する
- 上記のエラー系がすべて error になることをテストで検証する
- テストは test/kvconf_tests.erl に追加されている

# validate_pkix_privkey_pem_file が秘密鍵の中身を検証しないのを修正する

- Created: 2026-07-31
- Completed: YYYY-MM-DD
- Branch: feature/fix-validate-pkix-privkey
- Polished: 2026-07-31

## 目的

#kvc_pkix_privkey_pem_file のバリデーションが PEM エントリのタイプも DER 本体も検証せず、証明書ファイルや壊れた鍵を秘密鍵として受理してしまう。正しく検証する。

## 現状

- kvconf_pkix の validate_pkix_privkey_pem_file は PEM エントリを {_PkiAsn1Type, _Der, not_encrypted} のパターンマッチで受け、タイプと内容を一切確認しない
- public_key:pem_decode は base64 デコードとヘッダ変換のみで DER の正当性を検査しない
- 実測: 証明書ファイル / SubjectPublicKeyInfo のみのファイル / base64 がゴミ（"AAAA"）のファイル / 秘密鍵 + 証明書の 2 エントリ混在ファイルがすべて {ok, ...} で受理される
- 同一モジュールの validate_pkix_fullchain_pem_file と validate_pkix_cert_pem_file は 'Certificate' タイプと public_key:pkix_decode_cert による DER 検証を行っており、非対称
- 2 エントリ許容は 2021.5.2 のタイプ限定（'EcpkParameters' + 'ECPrivateKey'）に 2021.5.3 の順序自由化を合わせた仕様だったが、現行実装は任意の 2 エントリを受理するワイルドカードに逸脱している

## 設計方針

- PEM エントリタイプが秘密鍵系であることを確認する
  - 1 エントリ版は 'PrivateKeyInfo' / 'ECPrivateKey' / 'RSAPrivateKey' を許容する（'PrivateKeyInfo' は中身のアルゴリズムを問わず許可する。Ed25519 等も通る）。許容リストは TLS サーバ秘密鍵として現実に使われる形式であり、'DSAPrivateKey' 等の伝統的形式は意図的に error にする
  - 2 エントリ版は 'EcpkParameters' + 'ECPrivateKey' の組み合わせのみ許容する（順序はどちらでもよい。2021.5.2 のタイプ限定に 2021.5.3 の順序自由化を合わせた仕様に戻す。EC PARAMETERS のタイプ名は 'EcpkParameters' であり 'ECParameters' ではない）
  - 1 エントリの 'EcpkParameters' のみのファイルは秘密鍵を含まないため error にする
  - タイプ確認だけではゴミ base64 を弾けない（"AAAA" は 'PrivateKeyInfo' として返る）ため、der_decode による検証が必須
- すべてのエントリの DER を public_key:der_decode でデコードして正当性を検証する（2 エントリ版は 'EcpkParameters' 側も含む）。der_decode は成功時に生タプルを返し {ok, ...} で包まず、失敗時は例外（error:{badmatch, ...} / throw({error, {asn1, ...}})）を投げるため、validate_pkix_fullchain_pem_file の pkix_decode_cert と同様に try-catch で捕捉する
- 暗号化された秘密鍵 PEM（3 要素目が暗号化情報）は従来どおり error にする
- 数学的な鍵の妥当性（e / d / n の整合性等）は検証対象外とする
- pem_decode / der_decode の例外のエラー返却化は 0001（クラッシュ経路のエラー返却化）の範囲であり、本 issue はエントリタイプの確認と DER デコードの成功可否の検証のみを行う。0001 の関数全体 try-catch が der_decode の例外もカバーする

## 完了条件

- 以下が error になる
  - 証明書ファイル / SubjectPublicKeyInfo のみのファイル
  - 'EcpkParameters' 以外の秘密鍵でないタイプを含む混在ファイル（例: 秘密鍵 + 証明書の 2 エントリ）
  - 1 エントリの 'EcpkParameters' のみのファイル
  - 'EcpkParameters' + 'ECPrivateKey' 以外の組み合わせの 2 エントリファイル
  - 3 エントリ以上のファイル
  - 壊れた DER のファイル（例: PRIVATE KEY ラベルにゴミ base64 を含むファイル）
  - 暗号化された秘密鍵 PEM
- 正常な秘密鍵が {ok, ...} になる
  - PKCS#8（'PrivateKeyInfo'。RSA / EC / Ed25519 等の中身を問わない）
  - EC 1 エントリ（'ECPrivateKey'）
  - RSA 1 エントリ（'RSAPrivateKey'）
  - EC 2 エントリ（'EcpkParameters' + 'ECPrivateKey' の順序どちらも）
- 回帰テストが追加されている（本 issue は正常系とタイプ検証の回帰テストを追加し、エラー系のテストは 0013 が担う）。テスト用秘密鍵はリポジトリにコミットせず、0013 と同様にテスト実行時に生成する
- なお 0001 実装後は、上記の error ケースは {error, {error, ...}} を返す

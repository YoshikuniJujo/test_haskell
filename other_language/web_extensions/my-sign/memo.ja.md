memo
====

class EncryptedSecretKey
-------------------------

### NIP-49

```
CIPHERTEXT_CONCATENATION = concat(
        VERSION_NUMBER, LOG_N, SALT, NONCE, ASSOCIATED_DATA, CIPHERTEXT )
```

background.js
--------------

### API

#### get-account

##### 使用者

content.js

##### 役割

クライアントのページにアカウントを表示するための情報を提供する

##### 機能

URLからクライアントを検索する。
クライアントの公開鍵からアカウントを検索する。
以下の情報を返す

* アカウント名
* 公開鍵
* 表示領域の位置
* 表示領域の色と不透明度

#### get-public-key

##### 使用者

content.js

##### 役割

window.nostr.getPublicKeyでクライアントページに公開鍵を渡す

##### 機能

URLからクライアントを検索し、それの公開鍵を取り出す。
取り出せたら16進法で文字列に変換してクライアントページにそれを渡す

対応するクライアントが存在しなかった場合には設定ページを開く

#### queryPswd

#### returnPswd

#### contentStarted

#### sign-event

#### clientChanged

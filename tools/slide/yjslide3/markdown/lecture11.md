第11回 代数的データ型
=====================

はじめに
--------

今までは用意された型に対する演算を扱ってきたが、
Haskellでは自分で型を作ることができる。
型の作りかたには以下の方法がある。

* 値を列挙する
* 既存の型を組み合わせる

Haskellでは両者が無理なく1つの枠組みにまとまっている。
すくなくとも、意味論的には整数や文字型は、
値を列挙することによって作られた型と考えられる。

列挙する
--------

### はじめに

値を列挙することで型を作ることができる。
「値」は大文字ではじまる識別子である。

### 友達と性別の例

    data Friend = Takashi | Kazuya | Keiko
    data Gender = Man | Woman

3人の友達を表現する型Friendと性別を表現する型Genderとを作った。
次は、これらの型を扱う関数genderを作る。

    gender :: Friend -> Gender
    gender Takashi = Man
    gender Kazuya = Man
    gender Keiko = Woman

これらをlectures/lecture11/friend.hsに書きこむ。

男女を判定しメッセージを作成する関数を作る。

    genderCheck :: Friend -> String
    genderCheck f = case gender f of
        Man -> "He is a man."
        Woman -> "She is a woman."

これもlectures/lecture11/friend.hsに書きこみ、試してみる。

    % ghci friend.hs
    *Main> genderCheck Takashi
    "He is a man."
    *Main> genderCheck Keiko
    "She is a woman."

### 用意されている型

#### はじめに

Haskellでははじめから用意されている型としてBool型、Int型、Char型、等々がある。
これらの型は意味論的には列挙によって作られた型と考えられる。
ただし、実装としてはより効率的に作られている。

#### Bool型

    data Bool = False | True

Bool型は構文ではなく単なるライブラリとして作ることができる。

#### Int型

    data Int = -2147483648 | -2147483647 | ... | - 2 | - 1
             | 0 | 1 | 2 | ... | 2147483646 | 2147483647

#### Char型

    data Char = '\0' | '\1' | ... | 'A' | 'B' | ... | 'a' | 'b' | ...

既存の型を組み合わせる
----------------------

### はじめに

既存の型を組み合わせて新しい型を作る例を見ていく。
座標上の点についての例を見る。
まずはタプルによる実装を試し、問題点を示す。
その実装と代数的データ型により新しい型を作る方法とを比較する。

### タプルによる実装

#### 直交座標

直交座標上の位置(x, y)を以下のようにタプルで表現する。

    type Rect = (Double, Double)

これをベクトルと見てn倍する関数を考える。

n = 3の例を以下に示す。

![Haskell.B.Curry](mulRectImage.png "large")

x, yをともにn倍すれば良いので以下のようになる。

    mulRect :: Rect -> Double -> Rect
    mulRect (x, y) n = (x * n, y * n)

これらをcoordinate.hsに書きこみ、試してみる。

    *Main> :load coordinate.hs
    *Main> mulRect (4, 6) 3
    (12.0,18.0)

#### 極座標

極座標による位置の表現は以下のようになる。

    (原点からの距離, x軸からの角度)

これをタプルで表現すると

    type Pol = (Double, Double)

(dist, rad)の場合の例を以下に示す。

![image2](polImage.png "large")

前の例と同様にn倍する関数を書く。
原点からの距離をn倍すれば良いので

    mulPol :: Pol -> Double -> Pol
    mulPol (dist, rad) n = (dist * n, rad)

これらをcoordinate.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> mulPol (8, pi / 6) 5
    (40.0,0.5235987755982988)

#### 点を定義する

以下の定義をcoordinate.hsに書きこむ。

    pa :: Rect
    pa = (3, 8)

    pb :: Pol
    pb = (7, pi / 6)

#### 試してみる

    *Main> :reload
    *Main> pa
    (3.0,8.0)
    *Main> mulRect pa 3
    (9.0,24.0)
    *Main> pb
    (7.0,0.5235987755982988)
    *Main> mulPol pb 3
    (21.0,0.5235987755982988)

#### おかしな変換

ここまではうまくいっている。
しかし、以下の例を見てみよう。

    *Main> pb
    (7.0,0.5253987755982988)
    *Main> mulRect pb 4
    (28.0,2.0973951023931953)
    *Main> pa
    (3.0,8.0)
    *Main> mulPol pa 4
    (12.0,8.0)

極座標として定義したpbに直交座標用の関数を、
また、直交座標として定義したpaに極座標用の関数を適用してしまっている。

#### 問題点

typeによって定義される名前は型の「別名」なので、それぞれの型は区別されない。
RectとPolは同じモノに対して、単に、別の名前をつけているだけである。

    type Rect = (Double, Double)
    type Pol = (Double, Double)

本当なら、mulRectはRectにだけ使えてPolには使えず、
mulPolはPolにだけ使えてRectには使えない、というようにしたい。

そのためには、別の型として区別されるRect型、Pol型を作る必要がある。

### 代数的データ型で型を区別する

#### 型の定義

それぞれの座標型を作る。

    data Rect = Rect Double Double
    data Pol = Pol Double Double

このようにdataを使って定義すると、
この2つの型は完全に区別されるようになる。

また、'='の左側で新たに定義される名前(Rect, Pol)は型構築子であり、
右側で新たに定義される名前(Rect, Pol)は値構築子である。
両者は別のモノであるが、名前空間が違うので、同じ名前とすることができる。
もちろん別の名前でも良い。

#### 関数定義

##### 直交座標

##### 極座標

#### 試してみる

#### 型のミスマッチ

#### まとめ

### 代数的データ型で型をまとめる

#### はじめに

#### 型の定義

#### 関数定義

#### 試してみる

代数的データ型の構文
--------------------

まとめ
------

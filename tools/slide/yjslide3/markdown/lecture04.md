第4回 リスト
============

はじめに
--------

プログラミングにおける機能として「くりかえし」は重要である。
「くりかえし」を実現するために手続き型言語では状態変化を利用する。
Haskellでそれを実現するためにはリストを使うことができる。

くりかえし
----------

0からnまでの二乗の和を求めることを考える。

### 手続き型言語の方法

1. iに0を代入、sに0を代入
2. iがn以下のあいだ3, 4をくりかえす
3. sにiの二乗を足す
4. iに1を足す
5. sを返す

### リストを使った方法

1. 0からnまでの数のリストがあり
2. そのリストの値のすべてを二乗したリストがあれば
3. そのリストの要素の総和が求めるものである

### 比較

手続き型のほうは本当に正しいのかどうかは自明ではない。
機械の気持ちになって手続きを追うことが必要。
「iがn以下のとき?それともiがn+1以下のとき?」等々。

リストを使ったやりかたのほうは、手続きではなく構造を記述している。
アルゴリズムが正しいことは自明である。

高階関数
--------

リストを使った方法の2.の「リストの値のすべてを二乗したリスト」を作ることを考える。

「リストの値のすべてに何かする」という構造が抽出できる。
多くの言語でこのような「構造」は「構文」として特別扱いとなる。
C言語のfor文等がその例である。

引数として関数をとる関数を作れる言語では、
そのような「構造」を関数として定義することが可能となる。
引数として関数をとる関数は高階関数と呼ばれる。
「リストの値のすべてに何かする」関数は用意されている。

map関数がそれであり、map f lstでlstのすべての要素に関数fを適用できる。

関数mapを使う
-------------

### 関数square

まずはsquare関数を書く。(Intは処理系依存の整数型)。

    % cd ~/lectures/lecture04
    % nano -w squareSum.hs
    square :: Int -> Int
    square x = x ^ 2

試してみよう。

    % ghci squareSum.hs
    *Main> square 7
    49

作った関数をghciで確認しながら開発を進めるやりかたを採る。
関数が「だいたい正しい」ことを確認しながら開発できる。
テスト駆動開発よりラフなやりかたではある。

### 関数squareAll

square関数を利用してsquareAllを作成する。
型Xのリストは[X]と表記される。
以下をsquareSum.hsに書き込もう。

    squareAll :: [Int] -> [Int]
    squareAll ns = map square ns

試してみる。

    *Main> :reload
    *Main> squareAll [3, 2, 5, 8]
    [9, 4, 25, 64]

squareSumの完成
---------------

### 0からnまでのリスト

0からnまでのリストを作るのに[x .. y]という構文が使える。

これを使うと0からnまでの数を二乗した数のリストは以下のようになる。
以下をsquareSum.hsに書き込もう。

    squares :: Int -> [Int]
    squares n = squareAll [0 .. n]

### 総和を求める関数

総和を求める関数として関数sumが用意されている。

これを使うと最終的な関数は以下のようになる。
以下をsquareSum.hsに書き込もう。

    squareSum :: Int -> Int
    squareSum n = sum (squares n)

試してみる。

    *Main> :reload
    *Main> squareSum 4
    30

### 完成したファイル

完成したファイルは以下のようになっているはずだ。

    square :: Int -> Int
    square x = x ^ 2

    squareAll :: [Int] -> [Int]
    squareAll ns = map square ns

    squares :: Int -> [Int]
    squares n = squareAll [0 .. n]

    squareSum :: Int -> Int
    squareSum n = sum (squares n)

### 一行で書ける

関数を積み上げていく開発のやりかたを示した。
慣れてくるとはじめから一行で書ける。

    squareSum n = sum $ map (^ 2) [0 .. n]

「0からnまでのリストのすべての要素を二乗したものの総和」と
そのまま読むことができる。

空間効率
--------

経験をつんだプログラマならば気になる点がある。
「いちいちリストを作っていたら空間効率がひどいことになる」。
それはたしかに正しい。

しかし、Haskellのリストは遅延リストである。
よって、必要になるまでリストの要素は作られない。
また、2度と使われない要素はGC(ガベージコレクション)される。

よって、空間効率の低下はない。

プログラマの目から見れば「関数のあいだでリストを渡していく形」で
プログラムが組める。
しかも、実際にはそのプログラムは「くりかえし」と同じように動作する。

「くりかえし」の構造
--------------------

squareSumの例を作るなかで「くりかえし」の構造を3つの部分に分けた。

1. リストを作り出し ([0 .. n])
2. 要素すべてに関数を適用し (map square)
3. それらの要素をまとめあげた (sum)

それぞれの段階を
enumerate(数えあげ), map(写像), accumulate(集約)と呼ぶ。

験を担ぐ上司
------------

### 二等の和から4と9を除く

squareSumを上司のところへ持っていったらこう言われた。
「バカモノ!4と9は縁起が悪い。二乗の和に含めるな。」

0からnまでの4と9以外の数の二乗の和を求めよう。
squareSum.hsに以下を追加する。

    lucky :: Int -> Bool
    lucky 4 = False
    lucky 9 = False
    lucky _ = True

squareSum'を書いてみよう。以下をsquareSum.hsに追加する。

    squareSum' :: Int -> Int
    squareSum' n = sum $ map (^ 2) $ filter lucky [0 .. n]

filterという新しい関数がでてきた。これは一体何だろう。
型は以下のようになっている。

    filter :: (a -> Bool) -> [a] -> [a]

第一引数で指定した関数でリストの要素をチェックして、
結果が真のものだけを集めたリストを返す。

試してみよう。

    *Main> :reload
    *Main> squareSum 10
    385
    *Main> squareSum' 10
    288

4の二乗が16で9の二乗は81でその和は97となる。
385 - 288 = 97なのでちゃんと4と9の二乗は除かれている。

### 和から4と9を除く

squareSum'を上司のところへ持っていったらこう言われた。
「ふーん、いいね」
「あ、二乗して4と9になる数も縁起悪いよね」
「直しといて」

以下の関数を作ろう。
squareSum.hsに書き込む。

    squareSum'' :: Int -> Int
    squareSum'' n =
            sum $ filter lucky $ map (^ 2) $ filter lucky [0 .. n]

試してみる。

    *Main> :reload
    *Main> squareSum' 10
    288
    *Main> squareSum'' 10
    275

288 - 275 = 13なので4と9が除かれているのがわかる。

「くりかえし」の構造
--------------------

### 前に見た構造

「くりかえし」という動作を3つの部分に分けた。

1. リストを作り出し
2. 要素すべてに関数を適用し
3. それらの要素をまとめあげた

### filterの追加

さらに「要素のうちの条件を満たすものだけを取り出す」という段階を追加した。
これをfilter(選抜)と呼ぶ。

1. リストを作り出し (enumerate)
2. 写像(map)/選抜(filter)が複数回行われ
3. それらの要素をまとめあげる

「くりかえし」のまとめ
----------------------

* リストを使えば状態変化なしで「くりかえし」が可能
* 多くの「くりかえし」は以下のような構造を持つ
    - リストを作り (enumerate)
    - 要素を選び (filter)
    - それぞれの要素を変換し (map)
    - それをまとめる (accumulate)
* リストという「実体」を渡していくというモデルはわかりやすい
* 遅延リストとGCによって空間効率は問題なく保たれる

データ構造としてのリスト
------------------------

「くりかえし」の実体化としてのリストを見てきたが、
リストはデータ構造としても使える。
リストを扱う関数は数多く用意されているので、
データ構造としてリストを選択するのは多くの場合において適切である。

### 文字列

Haskellにおけるデフォルトの文字列は文字のリストなので、
リストを扱う豊富な関数はそのまま文字列に適用することができる。

今まで見てきた文字列リテラルは構文糖であり以下のように脱糖される。

    "hello"
    -> ['h', 'e', 'l', 'l', 'o']

試してみる。

    *Main> "hello"
    "hello"
    *Main> ['h', 'e', 'l', 'l', 'o']
    "hello"

Haskellは文字のリストを文字列として表示してくれる。
表示は文字列であるが中身は単なる文字のリストである。

### 注意点

* リストは本質的には「くりかえし」の実体化である
* データ構造として使う場合には不得意な分野がある
* ランダムアクセスには向いていない
    - リストは前から作って前から消費するのに向いている
* 大きなリストを保存しておくのには向かない
    - 順にGCされていかないような場合には空間効率が悪化する
* リストを使うのは以下のいずれかの場合が望ましい
    - 1度だけの「くりかえし」
    - 小さなリスト
    - 効率を考慮しないプロトタイプの作成

まとめ
------

* リストは「くりかえし」の実体化である
* 簡易的に使えるデータ構造としての側面もある
* Haskellでの標準的な文字列は「文字のリスト」
* データ構造としてのリストは使いどころを選ぶ
    - 大量の要素の保存には向かない
    - ランダムアクセスには向かない
    - プロトタイプであればそれらにも使うこともある

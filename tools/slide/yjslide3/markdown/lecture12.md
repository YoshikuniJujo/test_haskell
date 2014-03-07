第12回 多相型と再帰的な型
=========================

はじめに
--------

前回は代数的データ型について見た。
以下のような構文を学んだ。

    data [型構築子]
        = [値構築子1] [型11] [型12] ...
        | [値構築子2] [型21] [型22] 彼
	...

今回はこれを拡張し、より柔軟性のある定義をする。
それは、見方を変えると「構造」を抽象化した型と呼べる。

また、再帰的な型についても見ていく。

型変数
------

型を宣言する際には型変数が使える。
普通の変数と同じように同じ型変数aには同じ型が入る。
idの例を見てみよう。

    id :: a -> a

以下はidの型として正当である。

    Int -> Int
    Char -> Char
    (Int -> Int) -> (Int -> Int)
    [Char] -> [Char]
    ...

以下はidの型にはならない。

    Int -> Char
    (Int -> Int) -> Int
    [Char] -> Char
    ...

型変数を使うことで、
複数の型を同時に定義していると考えることができる。

以下の宣言は

    id :: a -> a

以下の宣言すべてを同時に行っていることになる。

    Int -> Int
    Char -> Char
    ...

多相型
------

### Twice

#### 型の定義

同じ型の値を2個集めた値の型を作る。
このとき「型」は何でも良い。
何でも良いので、型変数を使うことになる。

    data Twice a = Twice a a

これは以下のすべてを定義したのと同じことである。

    data Twice Int = Twice Int Int
    data Twice Char = Twice Char Char
    data Twice [Int] = Twice [Int] [Int]
    data Twice (Int -> Bool) = Twice (Int -> Bool) (Int -> Bool)
    ...

このように様々な型の総称となるような型を多相型と呼ぶ。

#### 関数の定義

Twiceの保持する2つの値に対して関数を適用する関数を書く。

    mapTwice :: (a -> b) -> Twice a -> Twice b
    mapTwice f (Twice x y) = Twice (f x) (f y)

ghciでの表示に対応するように型の定義に'deriving Show'をつける。

    data Twice a = Twice a a deriving Show

これらをlectures/lecture12/data.hsに書きこみ、試してみる。

    % ghci data.hs
    *Main> mapTwice even (Twice 3 8)
    Twice False True

### タプル

#### 型の定義

タプルと同等のものを自分で定義することができる。

    data Tuple a b = Tuple a b deriving Show

これをdata.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> Tuple 8 'c'
    Tuple 8 'c'

#### 関数の定義

自作のタプルの要素を取り出す関数を書く。

    myFst :: Tuple a b -> a
    myFst (Tuple x _) = x

    mySnd :: Tuple a b -> b
    mySnd (Tuple _ y) = y

data.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> myFst $ Tuple 3 'c'
    3
    *Main> mySnd $ Tuple 3 'c'
    'c'

#### 構文糖

前回まで見てきたタプルは実のところ構文糖が使われていた。
その構文糖は型についても値についても似たような形となっている。
構文糖を利用した形とそれを脱糖した形を示す。

* 型: (a, b) -> (,) a b
* 値: (x, y) -> (,) x y

脱糖した形でタプルを使用すると以下の例のようになる。

    someTuple :: (,) Int Char
    someTuple = (,) 3 'c'

つまり、タプルは以下のように定義されていることになる。

    data (,) a b = (,) a b

この定義は、特殊な識別子(,)が使われていること以外は、
自分で定義したTuple型の定義と同じことである。

つまり、タプルというデータ構造は特殊な名前と構文糖以外については、
代数的データ型の定義の範囲内で定義することができる。

### リスト

### 木

まとめ
------

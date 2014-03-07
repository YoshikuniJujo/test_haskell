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

### リスト

### 木

まとめ
------

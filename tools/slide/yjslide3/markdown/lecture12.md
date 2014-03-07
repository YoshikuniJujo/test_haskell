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

#### 型の定義

リストに関してもタプルと同様に自分で定義することができる。

    data List a = Cons a (List a) | Nil deriving Show

data.hsに書きこんで試してみる。

    *Main> :reload
    *Main> 3 `Cons` (4 `Cons` (7 `Cons` Nil))
    Cons 3 (Cons 4 (Cons 7 Nil))

Listに対するmap関数を書いてみる。

    mapList :: (a -> b) -> List a -> List b
    mapList _ Nil = Nil
    mapList f (x `Cons` xs) = f x `Cons` mapList f xs

data.hsに書きこんで、試してみる。

    *Main> :reload
    *Main> mapList even $ 3 `Cons` (4 `Cons` (7 `Cons` Nil))
    Cons False (Cons True (Cons False Nil))

#### 構文糖

リストについても構文糖がある。
構文糖を使った表現を脱糖すると以下のようになる。

* 型: [a] -> [] a
* 値: [x, y, z] -> x : y : z : []

つまりリストは以下のように定義されていると考えられる。

    data [] a = (:) a ([] a) | []

自分で定義したList型と比較すると、
Listは[]に、Consは(:)に、Nilは[]に対応しているのがわかる。

#### 再帰的な型

リストの定義をよく見ると、([] a)の定義のなかに([] a)自身が出てくる。
つまり、再帰的な定義となっている。

型の定義自体に構文糖を適用してみるとわかりやすくなる。

    data [a] = a : [a] | []

「aのリストはaのリストの先頭にaの値を足したもの、または空リスト」となる。

### 木

#### 型の定義

再帰的な型の別の例として「木」を考える。
簡単のために二分木としよう。

![binTreeImage](binTreeImage.png "large")

二分木の定義は以下のようになる。

    data BinTree a
        | Node (BinTree a) (BinTree a)
        | Leaf a
        deriving Show

二分木は左右に二分木をとる節(ノード)または値を持つ葉である。

これをdata.hsに書きこむ。

#### 木の例

上図の木を作ってみる。

    tree1 :: BinTree Char
    tree1 = Node
        (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c')))
        (Leaf 'd')

data.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> tree1
    Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

#### 関数の定義

葉の値をリストとして列挙する関数を書く。

    enumLeaf :: BinTree a -> [a]
    enumLeaf (Node l r) = enumLeaf l ++ enumLeaf r
    enumLeaf (Leaf x) = [x]

以下のように読める。

* 節の「葉の列挙」は左右の木の葉の列挙を連結したものである
* 葉の「葉の列挙」はその値ひとつをリストにしたものである

enumLeafの定義をdata.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> enumLeaf tree1
    "abcd"

まとめ
------

代数的データ型の構文には型変数が使える。
型変数を使うことでより柔軟な型の定義ができるようになる。
型変数を使った定義は様々な型を一度に定義していると考えることができる。
これは見方を変えると「構造」を定義しているとも考えられる。

例としてまずはタプルを見た。

次に代数的データ型で再帰的な型を定義できることを見た。
その例としてリストと木を定義した。
木の葉を列挙する関数を定義することで、再帰的なデータを扱う方法を学んだ。

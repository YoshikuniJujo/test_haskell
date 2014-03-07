第14回 ファンクター
===================

はじめに
--------

多相型に対する型クラスを作ることができる。
型変数をひとつとる多相型はコンテナと考えることができる。
コンテナとは値を格納するものである。

* Maybe型: 0または1個の値を格納する
* リスト: 複数の値を順番に格納する
* 木: 複数の値を木構造として格納する

これらのコンテナの構造そのものの性質を表現できる。
Functorはそのような型クラスのひとつであり、
「中身に関数を適用できる」という性質を型クラスにした。

それぞれのmap
-------------

### リスト

リストの中身の値に関数を適用する関数は

    map :: (a -> b) -> [a] -> [b]

lectures/lecture14ディレクトリを作ってそこに移動しよう。

試してみる。

    Prelude> map even [1 .. 5]
    [False,True,False,True,False]

### Maybe

Maybe型の中身の値に関数を適用する関数を書く。

    appMaybe :: (a -> b) -> Maybe a -> Maybe b
    appMaybe _ Nothing = Nothing
    appMaybe f (Just x) = Just $ f x

functor.hsに書きこみ、試してみる。

    Prelude> :load functor.hs
    *Main> appMaybe even Nothing
    Nothing
    *Main> appMaybe even (Just 4)
    Just True

### 木

まずは二分木を作る。

    data BinTree a
        = Node (BinTree a) (BinTree a)
        | Leaf a
        deriving Show

二分木のすべての葉に関数を適用する関数は以下のようになる。

    mapTree :: (a -> b) -> BinTree a -> BinTree b
    mapTree f (Leaf x) = Leaf $ f x
    mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

これをfunctor.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> let t = Node (Leaf 3) (Node (Leaf 4) (Leaf 5))
    *Main> mapTree even t
    Node (Leaf False) (Node (Leaf True) (Leaf False))
    *Main> mapTree (* 3) t
    Node (Leaf 9) (Node (Leaf 12) (Leaf 15))

型の比較
--------

それぞれの型に対するmap的な関数を見てきた。

    map :: (a -> b) -> [] a -> [] b
    appMaybe :: (a -> b) -> Maybe a -> Maybe b
    mapTree :: (a -> b) -> BinTree a -> BinTree b

これらの型を比較してみると以下の共通した形があることがわかる。

    (a -> b) -> f a -> f b

fにそれぞれの型を入れてみるとよくわかる。

型fの「中身に関数を適用可」という性質を言い換えると
「(a -> b) -> f a -> f bという型の関数を持つ」となる。

Functorクラス
-------------

### 定義

「中身に関数を適用可」を表すFunctorクラスがある。

    class Functor f where
        fmap :: (a -> b) -> f a -> f b

### リストとMaybe

リストやMaybeはFunctorクラスのインスタンスであり、
リストに対するfmapはmapと同じであり、
Maybeに対するfmapはappMaybeと同じである。

試してみる。

    *Main> fmap even [1 .. 5]
    [False,True,False,True,False]
    *Main> fmap even $ Just 8
    Just True

### 木

BinTreeは自作のデータ型なのでまだFunctorクラスのインスタンスではない。
BinTreeをFunctorクラスのインスタンスにする。

    instance Functor BinTree where
        fmap = mapTree

これをfunctor.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> let t = Node (Leaf 3) (Node (Leaf 4) (Leaf 5))
    *Main> fmap even t
    Node (Leaf False) (Node (Leaf True) (Leaf False))

toCodeの例
----------

それぞれのmap的な関数をfmapにまとめることで何かいいことがあるのだろうか。
その答えについて見ていこう。

値が何らかの構造のなかに存在する場合に、
その構造とは関係なく値に何かしたい場合がある。
例えば文字が入っている「何か」に対して、
その文字を「文字コード」に変えたいとする。

そのとき、その「何か」がFunctorであれば以下が使える。

    toCode :: Functor f => f Char -> f Int
    toCode = fmap ord

これをfunctor.hsに書きこむ。
Data.Charモジュールのordを使うので以下をfunctor.hsの先頭に追加しよう。

    import Data.Char (ord)

試してみる。

    *Main> :reload
    *Main> toCode ['a', 'b', 'c']
    [97,98,99]
    *Main> toCode $ Just 'd'
    Just 100
    *Main> let t = Node (Leaf 'e') (Node (Leaf 'f') (Leaf 'g'))
    *Main> toCode t
    Node (Leaf 101) (Node (Leaf 102) (Leaf 103))

様々なデータ構造において、その中身に同じ変換を行うことができる。

まとめ
------

多相型に対する型クラスの例としてFunctorを取り上げた。
Functorは「中身に対して関数適用可」を表す型クラスである。
Functorが持つ型クラスはfmapひとつである。

    fmap :: (a -> b) -> f a -> f b

これはfという構造のなかの型aの値に関数を適用する関数である。

Functorによって抽象化された構造を使えば、
コンテナの種類にかかわらず、値に対する操作を定義することができる。

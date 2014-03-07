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

Functorクラス
-------------

### リストとMaybe

### 木

toCodeの例
----------

まとめ
------

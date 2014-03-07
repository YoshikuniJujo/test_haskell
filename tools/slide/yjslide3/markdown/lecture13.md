第13回 型クラス
===============

はじめに
--------

複数の型に共通の性質が存在する。

* 大小比較ができる
* 文字列として表示できる
* 等々

ある型がその「性質」を持つということは、
その「性質」を表現する関数がその型に対して定義されている
ということと等しい。

つまり、(>)や(<)といった関数がIntに対して定義されているので、
Int型は「大小比較可」という性質を持つことになる。

型クラスとは
------------

Haskellではそのような「性質」を型クラスで表現する。
型TがクラスCで表される性質を持つということを
「型TはクラスCのインスタンスである」と表現する。

型クラスには以下のようなものがある。

* Ordは「大小比較可」という性質を表すクラスである
* Showは「表示可」という性質を表すクラスである

型クラスとインスタンスの例には以下のようなものがある。

* Int型はOrdクラスのインスタンスである
* Char型はShowクラスのインスタンスである
* 等々

対話環境における確認
--------------------

lectures/lecture13を作りそこに移動する。

### 型がどの型クラスに属するか

    % ghci
    Prelude> :info Char
    data Char = GHC.Types.C# GHC.Prim.Char#        -- Defined in `GHC.Types'
    instance Bounded Char -- Defined in `GHC.Enum'
    instance Enum Char -- Defined in `GHC.Enum'
    instance Eq Char -- Defined in `GHC.Classes'
    instance Ord Char -- Defined in `GHC.Classes'
    instance Read Char -- Defined in `GHC.Read'
    instance Show Char -- Defined in `GHC.Show'

ある型がどのクラスのインスタンスであるかを調べるには、
ghciで:info [型名]とする。
ここでは、CharがOrdやShowのインスタンスであることがわかる。

### 型クラスの持つクラス関数

ある型クラスにどんなクラス関数があるかを知るには

    Prelude> :info Ord
    class Eq a => Ord a where
        compare :: a -> a -> Ordering
        (<) :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        (>) :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        max :: a -> a -> a
        min :: a -> a -> a
              -- Defined in `GHC.Classes'
    instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'
    instance (Ord a, Ord b) => Ord (Either a b)
      -- Defined in `Data.Either'
    instance Ord Integer -- Defined in `integer-gmp:GHC.Integer.Type'
    ...
    instance (Ord a, Ord b) => Ord (a, b) -- Defined in `GHC.Classes'
    instance Ord () -- Defined in `GHC.Classes'

Ordクラスのクラス関数にはcompare, (<)などがあることがわかる。
また、'Eq a =>'とあるのは
「Ordクラスのインスタンスであるためには
Eqクラスのインスタンスである必要がある」
ということ。

また「どの型がこの型クラスのインスタンスであるか」もわかる。

Size型をOrdクラスのインスタンスにする
-------------------------------------

deriving
--------

型クラスを作る: オートマトンの例
--------------------------------

まとめ
------

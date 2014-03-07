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

### はじめに

型クラスは、もちろん、自分で作れるが、
まずは自作の型を既存の型クラスのインスタンスにしてみる。

### Size型の定義

スターバックスのカップのサイズを表す型を作る。

    data Size = Short | Tall | Grande | Venti

これをclass.hsに保存する。

これは大小比較可能なのでOrdクラスのインスタンスにする。
Ordクラスのインスタンスにするには、
まずはEqクラスのインスタンスにする必要がある。

### Eqクラス

Eqクラスは同値かどうかを判定可能という性質を表すクラスである。

Eqクラスのクラス関数には(==), (/=)がある。
このクラスのインスタンスにするには(==)だけを定義すれば良い。
(==)を定義しておけば(/=)にはデフォルトの定義が使われる。

実行効率あるいはその他の理由で(/=)を別に定義することもある。

実際にSizeをEqクラスのインスタンスにしてみよう。

    instance Eq Size where
        Short == Short = True
        Tall == Tall = True
        Grande == Grande = True
        Venti == Venti = True
        _ == _ = False

これをclass.hsに書きこみ、試してみる。

    Prelude> :load class.hs
    *Main> Short == Short
    True
    *Main> Tall == Venti
    False
    *Main> Grande /= Grande
    False
    *Main> Venti /= Short
    True

### Ordクラス

Ordクラスには7つのクラス関数がある。

    compare, (<), (>=), (>), (<=), max, min

すべて定義しても良いが、compareまたは(<=)のどちらかを定義すれば、
他の関数はデフォルトの値が使われる。

SizeをOrdのインスタンスにする。

    instance Ord Size where
        Short <= _ = True
        _ <= Short = False
        Tall <= _ = True
        _ <= Tall = False
        Grande <= _ = True
        _ <= Grande = False
        Venti <= _ = True

これをclass.hsに書き込み、試してみる。

    *Main> :reload
    *Main> Short < Short
    False
    *Main> Grande <= Grande
    True
    *Main> Tall >= Venti
    False
    *Main> Venti > Short
    True

ここまでのまとめ
----------------

複数の型に共通した性質がある。
「性質」はその型を扱う関数によって表現される。
そのような性質、つまり関数をまとめたものが型クラスである。

型クラスのインスタンスにするには関数の中身を定義すれば良い。
構文は以下のようになる。

    instance [型クラス] [型名] where
        [関数定義1]
        [関数定義2]
        ...

deriving
--------

型SizeをOrdクラスのインスタンスにするのはもっと簡単にできる。

    data Size = Short | Tall | Grande | Venti deriving (Eq, Ord)

使用頻度の高い以下のクラスについてはderivingで簡単にインスタンスを導出できる。

    Eq, Ord, Enum, Ix, Bounded, Show, Read

derivingを使う場合は値構築子を「小さいものから大きいものへ」の順に
並べておくと良い。

型クラスを作る: オートマトンの例
--------------------------------

まとめ
------

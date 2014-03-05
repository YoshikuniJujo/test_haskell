演習10-22(解答)
===============

問い
----

関数zipWithはふたつのリストのそれぞれの要素を任意の関数でまとめる。
再帰を直接使って関数zipWithを定義せよ。

解説
----

* どちらかが空リストなら空リスト
* xsとysについての結果zsがあるなら
    + (x : xs)と(y : ys)に対してはf x y : zsとなる

解答
----

    zipWith _ [] _ = []
    zipWith _ _ [] = []
    zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

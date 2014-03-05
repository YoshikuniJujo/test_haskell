演習10-23(解答)
===============

問い
----

関数zipWithはふたつのリストのそれぞれの要素を任意の関数でまとめる。
unfoldrを使って関数zipWithを定義せよ。

解説
----

* 終了条件: どちらかのリストが空

(x : xs, y : ys)に対して

* 結果の値: f x y
* 次の値: (xs, ys)

解答
----

    zipWith f = curry $ unfoldr $ \lsts -> case lsts of
        (x : xs, y : ys) -> Just (f x y, (xs, ys))
        _ -> Nothing

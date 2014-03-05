演習10-7(解答)
==============

問い
----

foldrを使ってmapを定義せよ。

解説
----

(:)を置き換える「演算子op」を考えれば良い。
以下の二行を見くらべてみる。

    map f (x : xs) = f x : map f xs
    map f (x : xs) = x `op` map f xs

すると以下の関係がわかる。

       x `op` lst == f x : lst
    -> op = \x lst -> f x : lst
    -> op = \x -> (f x :)
    -> op = \x -> (:) f x
    -> op = (:) . f

解答
----

map f = foldr ((:) . f) []

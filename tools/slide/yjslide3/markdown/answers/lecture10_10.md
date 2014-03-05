演習10-10(解答)
===============

問い
----

foldrを使ってfilterを定義せよ。

解説
----

空リストは空リストに変換される。

    filter _ [] = []

リストが空じゃない場合は

    filter p (x : xs)
        | p x = x : filter p xs
        | otherwise = filter p xs

(:)を置き換える演算子を求めれば良いので、
以下の定義を満たすようなopを考える。

    filter p (x : xs) = x `op` filter p xs

よって、opは以下のようになる。

       op = \x xs -> if p x then x : xs else xs
    -> op = \x -> if p x then (x :) else id

解答
----

filter p = foldr (\x -> if p x then (x :) else id) []

演習10-16(解答)
===============

問い
----

関数zipはふたつのリストをそれぞれの要素のタプルから成るひとつの要素に
まとめる。
unfoldrを使って関数zipを定義せよ。

解説
----

* 終了条件: どちらかが空ならば空リストとなる
* 結果の値: それぞれのリストの頭をタプルにしたものが結果の値となる
* 次の値: 2つのリストの頭をとった残りが次にわたす値となる

よってunfoldrに与える関数は以下のようになる。

    f :: ([a], [b]) -> Maybe ((a, b), ([a], [b]))
    f (x : xs, y : ys) = Just ((x, y), (xs, ys))
    f _ = Nothing

fを以下のように無名関数とする。

    f = \lsts -> case lsts of
        (x : xs, y : ys) -> Just ((x, y), (xs, ys))
        _ -> Nothing

以下の2つの型をくらべてみる。

    unfoldr f :: ([a], [b]) -> [(a, b)]
    zip :: [a] -> [b] -> [(a, b)]

よって、前者をカリー化することで後者を導くことができる。

解答
----

    zip = curry $ unfoldr $ \lsts -> case lsts of
        (x : xs, y : ys) -> Just ((x, y), (xs, ys))
        _ -> Nothing

演習17-6 (解答)
===============

問い
----

以下のような関数doubleを考える。

    double :: Int -> Logger Int
    double 8 => ["double 8"] 16

関数doubleを定義せよ。

解答
----

    double :: Int -> Logger Int
    double n = tell ("double " ++ show n) >> return (n * 2)

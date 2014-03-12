演習17-3 (解答)
===============

問い
----

関数tellは文字列をログにする関数であり以下の型を持つ。

    tell :: String -> Logger ()

関数tellを定義せよ。

解答
----

    tell :: String -> Logger ()
    tell l = Logger [l] ()

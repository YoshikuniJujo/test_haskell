演習17-4 (解答)
===============

問い
----

型LoggerをMonadクラスのインスタンスにする。

まずはreturnを定義せよ。

解答
----

    return x = Logger [] x

解答2
-----

引数xは省略可。

    return = Logger []

インスタンス宣言
----------------

とりあえず(>>=)の定義は空白にしておく。

    instance Monad Logger where
        return = Logger []

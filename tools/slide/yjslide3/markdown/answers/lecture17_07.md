演習17-7 (解答)
===============

問い
----

関数toCodeDoubleは、
ログを記録しながら文字コードを2倍する。

関数toCodeDoubleをtoCodeとdoubleを使って定義せよ。

解答
----

    toCodeDouble :: Char -> Logger Int
    toCodeDouble c = toCode c >>= double

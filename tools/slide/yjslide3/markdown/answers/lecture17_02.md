演習17-2 (解答)
===============

問い
----

ログを取りつつ文字コードを求める関数

    toCode :: Char -> Logger Int

これは以下のような動作をする。

    toCode 'c' => Logger ["toCode 'c'"] 99

toCodeを定義せよ
(import Data.Char (ord)が必要)。

解答
----

    toCode :: Char -> Logger Int
    toCode c = Logger ("toCode " ++ show c) (ord c)

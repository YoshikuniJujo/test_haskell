演習19-3(解答)
==============

問い
----

"What's your name?"を出力し、キーボード入力を1行受け取り、
それをそのまま表示するプログラムを書け。

解答
----

    main :: IO ()
    main = do
        putStrLn "What's your name?"
        str <- getLine
        putStrLn str

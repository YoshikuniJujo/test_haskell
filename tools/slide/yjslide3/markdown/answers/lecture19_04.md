演習19-4(解答)
==============

問い
----

"What's your name?"を表示し、キーボード入力を1行受け取り、
入力された行の前後に"Hello, "と"!"をつけて表示するプログラムを作成せよ。

解答
----

    main :: IO ()
    main = do
        putStrLn "What's your name?"
        str <- getLine
        putStrLn $ "Hello, " ++ str ++ "!"

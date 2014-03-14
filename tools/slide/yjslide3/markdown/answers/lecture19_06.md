演習19-6(解答)
==============

問い
----

"What's your name?"と表示し、
入力行の前後に"Hello, "と"!"を追加して出力を、
空行が入力されるまでくりかえすプログラムを作成せよ。

解答
----

    main :: IO ()
    main = do
        putStrLn "What's your name?"
        str <- getLine
        if null str
            then return ()
            else do
                putStrLn $ "Hello, " ++ str ++ "!"
                main

解説
----

main自体を再帰的に使っている。

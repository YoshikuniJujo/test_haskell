演習19-5
========

問い
----

"What's your name?"を表示し、入力を1行受け取り、
その入力が空だったら何も表示せず、
そうでなければ入力された行の前後に"Hello, "と"!"を追加して出力するプログラムを
作成せよ。

解答
----

    main :: IO ()
    main = do
        putStrLn "What's your name?"
        str <- getLine
        if null str
            then return ()
            else putStrLn $ "Hello, " ++ str ++ "!"

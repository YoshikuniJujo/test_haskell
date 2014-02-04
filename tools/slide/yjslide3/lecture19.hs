import Lecture

subtitle :: String
subtitle = "第19回 mainと演習"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, aboutMain, aboutCompile,
	helloName, helloName2, helloName3, helloName4, helloName5, summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* モジュール宣言をつけないときのデフォルトのモジュール", \t -> do
	itext t 1 "Main", \t -> do
	text t "* そのMainモジュールのなかでmainという値を定義すると", \t -> do
	itext t 1 "- そのmainという値は特別な意味を持つ", \t -> do
	text t "* 今までは対話環境のなかで様々な値を評価してきた", \t -> do
	text t "* スタンドアロンなプログラムはまだ見ていない", \t -> do
	text t "* スタンドアロンなプログラムを作るうえで重要になるのが", \t -> do
	itext t 1 "- Mainモジュールのmainという値"
 ]

aboutMain :: Page
aboutMain = [\t -> do
	writeTopTitle t "main"
	text t "", \t -> do
	text t "* スタンドアロンなプログラムとは", \t -> do
	itext t 1 "- 入出力を行う機械と考えることができる", \t -> do
	text t "* その機械を定義するのがmainという値である", \t -> do
	text t "* Haskellで書かれたプログラムを動かすということは", \t -> do
	itext t 1 "- mainという名前に束縛された機械を「実行」すること", \t -> do
	text t "* プログラムはすでに完全に組み立てられている必要がある", \t -> do
	text t "* つまり、何も受け取らず何も渡さない機械なので", \t -> do
	itext t 1 "main :: IO ()"
 ]

aboutCompile :: Page
aboutCompile = [\t -> do
	writeTopTitle t "Hello, world!"
	text t "", \t -> do
	text t "* ghcによるコンパイルは簡単", \t -> do
	itext t 1 "% ghc [名前].hs", \t -> do
	text t "* ここまで学んできてようやく世界に挨拶ができる", \t -> do
	text t "* lectures/lecture19ディレクトリを作成し", \t -> do
	text t "* 以下の内容でhello.hsを作ろう", \t -> do
	itext t 1 "main :: IO ()", \t -> do
	itext t 1 "main = putStrLn \"Hello, world!\"", \t -> do
	text t "* コンパイル、実行", \t -> do
	itext t 1 "% ghc hello.hs", \t -> do
	itext t 1 "% ./hello", \t -> do
	itext t 1 "Hello, world!"
 ]

helloName :: Page
helloName = [\t -> do
	writeTopTitle t "Hello, you!"
	text t "", \t -> do
	text t "* 入力した名前に対して挨拶するプログラムを作っていこう", \t -> do
	text t "* 演習19-1. 入力した行をそのまま表示するプログラムを書け", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* できただろうか?", \t -> do
	itext t 1 "main :: IO ()", \t -> do
	itext t 1 "main = getLine >>= putStrLn", \t -> do
	text t "* 演習19-2. これをdo記法で書き直せ", \t -> do
	itext t 1 "(1分)"
 ]

helloName2 :: Page
helloName2 = [\t -> do
	writeTopTitle t "Hello, you!"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "main = do", \t -> do
	itext t 2 "str <- getLine", \t -> do
	itext t 2 "putStrLn str", \t -> do
	text t "* 演習19-3. 入力の前に\"What's your name?\"を表示せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "main = do", \t -> do
	itext t 2 "putStrLn \"What's your name?\""
	itext t 2 "str <- getLine"
	itext t 2 "putStrLn str"
 ]

helloName3 :: Page
helloName3 = [\t -> do
	writeTopTitle t "Hello, you!"
	text t "", \t -> do
	text t "* 演習19-4. 入力行の前後に\"Hello, \"と\"!\"をつけて表示せよ", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* 答え"
	itext t 1 "main = do", \t -> do
	itext t 2 "putStrLn \"What's your name?\""
	itext t 2 "str <- getLine"
	itext t 2 "putStrLn $ \"Hello, \" ++ str ++ \"!\""
 ]

helloName4 :: Page
helloName4 = [\t -> do
	writeTopTitle t "Hello, you!"
	text t "", \t -> do
	text t "* 演習19-5. 入力行が空だったら何も表示しないようにせよ", \t -> do
	itext t 1 "(null :: String -> Boolが使える)", \t -> do
	itext t 1 "(1分)", \t -> do
	text t "* 答え", \t -> do
	itext t 0 "main = do", \t -> do
	itext t 1 "putStrLn \"What's your name?\""
	itext t 1 "str <- getLine"
	itext t 1 "if null str"
	itext t 2 "then return ()"
	itext t 2 "else putStrLn $ \"Hello, \" ++ str ++ \"!\""
 ]

helloName5 :: Page
helloName5 = [\t -> do
	writeTopTitle t "Hello, you!"
	text t "", \t -> do
	text t "* 演習19-6. 「名前を尋ねて挨拶」をくりかえせ", \t -> do
	itext t 1 "(2分)", \t -> do
	text t "* これはちょっとした発想の転換が必要だったかもしれない", \t -> do
	itext t 0 "main = do"
	itext t 1 "putStrLn \"What's your name?\""
	itext t 1 "str <- getLine"
	itext t 1 "if null str"
	itext t 2 "then return ()"
	itext t 2 "else do"
	itext t 3 "putStrLn $ \"Hello, \" ++ str ++ \"!\""
	itext t 3 "main"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* スタンドアロンなプログラムの作りかたを見た", \t -> do
	text t "* Mainモジュールのmainの値を定義してやれば良い", \t -> do
	text t "* mainは「完成した機械」なので", \t -> do
	itext t 1 "main :: IO ()", \t -> do
	text t "* 文字列の入出力について演習問題を解いた", \t -> do
	text t "* mainを再帰的に使うことで「くりかえし」を定義した"
 ]

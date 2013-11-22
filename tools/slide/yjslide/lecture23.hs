import Lecture

subtitle :: String
subtitle = "第23回 例外処理"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, prelude2, prelude3
--	exceptionOccur, exceptionCatchAll,
--	selectException, selectException2, selectException3,
--	hierarchy
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 「例外的」なことが起きたときに「例外」が発生する", \t -> do
	text t "* 何が「例外的」かは考えかたにより異なってくるだろう", \t -> do
	text t "* 例えばゼロ除算は例外を発生させる", \t -> do
	itext t 1 "> 1 `div` 0"
	itext t 1 "*** Exception: divide by zero", \t -> do
	text t "* divの作者はゼロ除算を「例外的」であると考えたため", \t -> do
	itext t 1 "div :: Integral a => a -> a -> a", \t -> do
	text t "* そうでなければdivの型は以下のようだっただろう", \t -> do
	itext t 1 "div :: Integral a => a -> a -> Maybe a"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 例外の代わりにMaybeやEitherを使うことは可能", \t -> do
	text t "* Maybe等ではなく例外を利用するということは", \t -> do
	itext t 1 "- ゼロ除算はしないように気をつけよう", \t -> do
	itext t 1 "- ゼロ除算をした場合は警報を鳴らしますよ", \t -> do
	text t "* 例外は深くで生じたとしてもトップレベルにまでとどく", \t -> do
	text t "* 例外はどちらかのやりかたで使われるべき", \t -> do
	itext t 1 "- ユーザーにエラーを通知して終了", \t -> do
	itext t 1 "- 例外を適切に処理して仕事を続ける"
 ]

prelude3 :: Page
prelude3 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ユーザーにエラーを通知して終了", \t -> do
	itext t 1 "- 例外のデフォルトの動作", \t -> do
	itext t 1 "- 通知や終了のしかたを変更する場合", \t -> do
	itext t 2 "例外を捕捉し適切な処理を行う必要がある", \t -> do
	text t "* 例外を適切に処理して仕事を続ける", \t -> do
	itext t 1 "- 例外を捕捉し適切な処理を行う", \t -> do
	itext t 1 "- 例外の発生から捕捉までのコードは実行されない", \t -> do
	text t "* 例外を捕捉する方法について見ていこう"
 ]

prelude_ :: Page
prelude_ = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 例外処理について学ぶ", \t -> do
	text t "* 入出力には例外がつきものである", \t -> do
	text t "* また0除算などの例外もある", \t -> do
	text t "* 例外として補足可能なbottom値もある", \t -> do
	itext t 1 "- errorやundefined", \t -> do
	itext t 1 "- IO内に入れる必要がある", \t -> do
	itext t 1 "- ただし遅延評価ゆえの落とし穴に注意が必要", \t -> do
	text t "* 遅延IOにおける例外処理には注意が必要になる", \t -> do
	itext t 1 "- 遅延IOについてはここでは軽く触れるのみ", \t -> do
	itext t 1 "- 遅延IOにおける例外処理については上級編で"
 ]

exceptionOccur :: Page
exceptionOccur = [\t -> do
	writeTopTitle t "ファイルが存在しない"
	text t "", \t -> do
	text t "* 存在しないファイルを読む", \t -> do
	itext t 1 "> readFile \"notExist.txt\""
	itext t 1 "*** Exception: notExist.txt: openFile:"
	itext t 2 "does not exist (No such ...)", \t -> do
	text t "* とにかくこれを捕捉してみよう", \t -> do
	itext t 1 "> readFile \"notExist.txt\" `catch`"
	itext t 2 "\\(e :: SomeException) -> return \"\""
 ]

exceptionCatchAll :: Page
exceptionCatchAll = [\t -> do
	writeTopTitle t "すべての例外を捕捉"
	text t "", \t -> do
	text t "* catchの型を見てみよう", \t -> do
	itext t 1 "catch :: Exception e =>"
	itext t 2 "IO a -> (e -> IO a) -> IO a", \t -> do
	text t "* withLifeを思い出してみよう", \t -> do
	itext t 1 "withLife :: (Life l1, Life l2) =>"
	itext t 2 "l1 -> (l2 -> IO ()) -> IO ()", \t -> do
	text t "* すべてのLifeに対してIOアクションを行う", \t -> do
	itext t 1 "withLife life $ \\(l :: SomeLife) -> ...", \t -> do
	text t "* すべての例外をキャッチする", \t -> do
	itext t 1 "catch action $ \\(e :: SomeException) -> ..."
 ]

selectException :: Page
selectException = [\t -> do
	writeTopTitle t "複数の種類の例外"
	text t "", \t -> do
	text t "* 以下の例について考えてみる", \t -> do
	itext t 1 "printDivFileLen :: FilePath -> IO ()"
	itext t 1 "printDivFileLen fp = do"
	itext t 2 "cnt <- readFile fp"
	itext t 2 "print $ 100 `div` length cnt"
 ]

selectException2 :: Page
selectException2 = [\t -> do
	writeTopTitle t "複数の種類の例外", \t -> do
	text t "* 動作例", \t -> do
	itext t 1 "> readFile \"test.txt\""
	itext t 1 "\"abcde\\n\"", \t -> do
	itext t 1 "> printDivFileLen \"test.txt\""
	itext t 1 "16", \t -> do
	itext t 1 "> readFile \"empty.txt\""
	itext t 1 "\"\"", \t -> do
	itext t 1 "> printDivFileLen \"empty.txt\""
	itext t 1 "*** Exception: divide by zero", \t -> do
	itext t 1 "> printDivFileLen \"notExist.txt\""
	itext t 1 "*** Exception: notExist.txt: openFile:"
	itext t 2 "does not exist (No such File or directory)"
 ]

selectException3 :: Page
selectException3 = [\t -> do
	writeTopTitle t "複数の種類の例外"
	text t "", \t -> do
	text t "* 異なる種類の例外が発生する", \t -> do
	itext t 1 "- ファイルが存在しなかったときは入出力例外", \t -> do
	itext t 1 "- ファイルが空のときはゼロ除算例外", \t -> do
	text t "* どちらの例外も捕捉したい場合", \t -> do
	itext t 1 "- `catch` \\(e :: SomeException) -> ...とする", \t -> do
	text t "* 入出力例外だけをつかまえたい場合", \t -> do
	itext t 1 "- `catch` \\(e :: IOException) -> ...", \t -> do
	text t "* 演算例外だけをつかまえたい場合", \t -> do
	itext t 1 "- `catch` \\(e :: ArithException) -> ..."
 ]

hierarchy :: Page
hierarchy = [\t -> do
	writeTopTitle t "例外型の階層構造"
	text t "", \t -> do
	text t "* 例外型はSomeExceptionを最上位とした階層構造を取る", \t -> do
	text t "* 上位の型を指定すれば下位の型は全て捕捉される", \t -> do
	text t "* よってSomeExceptionの指定で全ての例外を捕捉"
 ]

import Lecture

subtitle :: String
subtitle = "第24回 例外処理"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, prelude2, prelude3,
	resource, resource2, finalizer, onError, finallySummary,
	syncAsync, syncAsync2, syncAsync3,
	useTry, useTry2, useTry3, useTry4, useTry5, useTry6, useTrySummary,
	useCatch, useCatch2, useCatch3, useCatch4, useCatch5, useCatchSummary,
	useCatches, useCatches2, useCatchesSummary,
	catchAll, catchAll2, catchAllSummary,
	myException
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

resource :: Page
resource = [\t -> do
	writeTopTitle t "リソースの解放"
	text t "", \t -> do
	text t "* 例外を捕捉したくなる場面にはいくつかある", \t -> do
	text t "* 例外が発生してから捕捉するまでのコードは実行されない", \t -> do
	text t "* 何らかのリソースを確保した場合", \t -> do
	itext t 1 "- それが解放されることを保証したい", \t -> do
	text t "* この場合、例外の捕捉は以下のようになる", \t -> do
	itext t 1 "- すべての例外を捕捉", \t -> do
	itext t 1 "- リソースの解放処理", \t -> do
	itext t 1 "- 同じ例外をもう一度投げ直す", \t -> do
	text t "* この枠組を抽象化した関数が用意されている", \t -> do
	text t "bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c"
 ]

resource2 :: Page
resource2 = [\t -> do
	writeTopTitle t "リソースの解放"
	text t "", \t -> do
	text t "* 使いかた", \t -> do
	itext t 1 "bracket ::"
	itext t 2 "IO a -> (a -> IO b) -> (a -> IO c) -> IO c"
	itext t 1 "bracket [確保] [解放] [行いたい処理]"
	text t "", \t -> do
	text t "例: bracket (openFile \"file\" ReadMode) hClose hGetLine", \t -> do
	text t "* openFileが返すハンドルがhCloseとhGetLineに渡される", \t -> do
	text t "* hGetLineで例外が生じた場合でもhCloseが行われる"
 ]

finalizer :: Page
finalizer = [\t -> do
	writeTopTitle t "終了処理"
	text t "", \t -> do
	text t "* リソースの解放に限らず、必ず行いたい処理はある", \t -> do
	itext t 1 "- 正常終了時でも例外発生時でも行いたい後処理", \t -> do
	text t "* bracketを単純化した関数", \t -> do
	itext t 1 "finally :: IO a -> IO b -> IO a"
	text t "", \t -> do
	text t "例: readFile \"test.txt\" `finally`"
	itext t 2 "putStrLn \"readFile end\""
 ]

onError :: Page
onError = [\t -> do
	writeTopTitle t "エラー処理"
	text t "", \t -> do
	text t "* エラーのときだけ行いたい処理もある", \t -> do
	text t "* finallyと似ているが正常終了時には後処理をしない", \t -> do
	itext t 1 "onException :: IO a -> IO b -> IO a"
	text t "", \t -> do
	text t "例: readFile \"test.txt\" `onException`"
	itext t 2 "putStrLn \"read error\""
 ]

finallySummary :: Page
finallySummary = [\t -> do
	writeTopTitle t "後処理(まとめ)"
	text t "", \t -> do
	text t "* 例外の発生にかかわらず必ず実行したい後処理", \t -> do
	itext t 1 "finally :: IO a -> IO b -> IO a", \t -> do
	text t "* 後処理のなかで典型的なものがリソースの解放処理", \t -> do
	itext t 1 "bracket ::"
	itext t 2 "IO a -> (a -> IO c) -> (a -> IO c) -> IO c", \t -> do
	text t "* 例外発生時にだけ行いたい後処理", \t -> do
	itext t 1 "onError :: IO a -> IO b -> IO a", \t -> do
	text t "* これらの後処理は同じ例外を再度発生させる"
 ]

syncAsync :: Page
syncAsync = [\t -> do
	writeTopTitle t "同期例外と非同期例外"
	text t "", \t -> do
	text t "* 例外には同期例外と非同期例外がある", \t -> do
	text t "* 例外を捕捉する場合、どちらの例外か考える必要がある", \t -> do
	text t "* 同期例外とは", \t -> do
	itext t 1 "- 例外が起こる場所が決まっている例外", \t -> do
	itext t 1 "- 例えば「ファイルが存在しない」という例外は", \t -> do
	itext t 2 "readFile等のなかで起こる", \t -> do
	text t "* 非同期例外とは", \t -> do
	itext t 1 "- 例外が起こる場所が決まっていない例外", \t -> do
	itext t 1 "- たとえばユーザーによるキーボード割り込みは", \t -> do
	itext t 2 "コードのどこで起こるかわからない"
 ]

syncAsync2 :: Page
syncAsync2 = [\t -> do
	writeTopTitle t "同期例外と非同期例外"
	text t "", \t -> do
	text t "* 同期例外の処理中に同期例外が起こらないようにする", \t -> do
	itext t 1 "- 同期例外の起こり得ない処理を使えば良い", \t -> do
	itext t 1 "- あるいは、同期例外をきちんと捕捉してやれば良い", \t -> do
	arrowIText t 1 "問題ない", \t -> do
	text t "* 非同期例外の処理中に非同期例外が起こらないようにする", \t -> do
	itext t 1 "- 非同期例外はどの処理をしていても生じ得る", \t -> do
	arrowIText t 1 "非同期例外の処理中に非同期例外が生じる可能性", \t -> do
	arrowIText t 1 "捕捉したはずの例外が外側にもれてしまう", \t -> do
	text t "* 非同期例外の処理中に非同期例外をブロックする必要性", \t -> do
	itext t 1 "- catch関数はそれを自動で行う"
 ]

syncAsync3 :: Page
syncAsync3 = [\t -> do
	writeTopTitle t "同期例外と非同期例外"
	text t "", \t -> do
	text t "* catch関数は例外処理を非同期例外をブロックして行う", \t -> do
	itext t 1 "- 非同期例外の処理には必要", \t -> do
	itext t 1 "- 同期例外の処理には不要", \t -> do
	text t "* 同期例外の処理にcatchを使った場合", \t -> do
	itext t 1 "- 不要なブロックが行われる", \t -> do
	itext t 1 "- コードの最適化(末尾呼び出し)が行われた場合", \t -> do
	itext t 2 "ブロックの範囲が広くなる", \t -> do
	arrowIText t 2 "長時間にわたって非同期例外に反応しない", \t -> do
	arrowIText t 2 "不要なパフォーマンスの低下", \t -> do
	text t "* よって、同期例外の捕捉にはcatch関数は使わない", \t -> do
	itext t 1 "- try関数を使う"
 ]

useTry :: Page
useTry = [\t -> do
	writeTopTitle t "同期例外の捕捉"
	text t "", \t -> do
	text t "* 同期例外の捕捉にはtry関数を使う", \t -> do
	itext t 1 "try :: Exception e => IO a -> IO (Either e a)", \t -> do
	text t "* 型がすべてを語っている", \t -> do
	text t "* aを返すIOを取って例外型の値またはaを返すIOに変える", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "> try $ readFile \"notExist.txt\""
	itext t 1 "No instance for (Exception e 0) arising from ..."
	itext t 1 "The type variable `e0' is ambiguous ...", \t -> do
	text t "* エラーになってしまった"
 ]

useTry2 :: Page
useTry2 = [\t -> do
	writeTopTitle t "同期例外の捕捉"
	text t "", \t -> do
	text t "* 「The type variable `e0` is ambigous」に注目", \t -> do
	text t "* tryの型を見てみよう", \t -> do
	itext t 1 "try :: Exception e => IO a -> IO (Either e a)", \t -> do
	text t "* 「Exception e =>」とある", \t -> do
	itext t 1 "- tryはExceptionクラスのメソッドを使っている", \t -> do
	itext t 1 "- メソッドはそれぞれの型ごとに異なる", \t -> do
	itext t 1 "- 型eが決まらないとtryの動作は決まらない", \t -> do
	text t "* 型eを決めてやれば良い", \t -> do
	itext t 1 "> try $ readFile \"notExist.txt\""
	itext t 2 ":: IO (Either IOException String)"
	itext t 1 "Left notEsist.txt: openFile: does not ..."
 ]

useTry3 :: Page
useTry3 = [\t -> do
	writeTopTitle t "同期例外の捕捉"
	text t "", \t -> do
	text t "* 型eをIOException型と指定した", \t -> do
	text t "* この指定では入出力例外のみを捕捉する", \t -> do
	text t "* よって以下の場合は例外は捕捉されない", \t -> do
	itext t 1 "> try $ print (1 `div` 0)"
	itext t 2 ":: IO (Either IOException ())"
	itext t 1 "*** Exception: divide by zero", \t -> do
	text t "* ゼロ除算例外はArithException型なので", \t -> do
	itext t 1 "> try $ print (1 `div` 0)"
	itext t 2 ":: IO (Either ArithException ())"
	itext t 1 "Left divide by zero", \t -> do
	text t "* これはLeft DivideByZeroということである"
 ]

useTry4 :: Page
useTry4 = [\t -> do
	writeTopTitle t "同期例外の捕捉"
	text t "", \t -> do
	text t "* ArithException型の値にはDivideByZero以外に", \t -> do
	itext t 1 "- RatioZeroDenominator等がある", \t -> do
	text t "* 例外を捕捉するということは", \t -> do
	itext t 1 "- 例外を「例外的」ではないと考えるということ", \t -> do
	itext t 1 "- その例外は予測可能であるということの表明", \t -> do
	itext t 1 "- 今回はDivideByZeroについて考えている", \t -> do
	text t "* DivideByZero以外の例外は捕捉しないほうが良い", \t -> do
	text t "* 捕捉する例外をしぼり込むための関数がある", \t -> do
	itext t 1 "tryJust :: Exception e =>"
	itext t 2 "(e -> Maybe b) -> IO a -> IO (Either b a)"
 ]

useTry5 :: Page
useTry5 = [\t -> do
	writeTopTitle t "同期例外の捕捉"
	text t "", \t -> do
	text t "* 捕捉する例外をしぼり込むための関数", \t -> do
	itext t 1 "tryJust :: Exception e =>"
	itext t 2 "(e -> Maybe b) -> IO a -> IO (Either b a)", \t -> do
	text t "* この関数は以下の仮想的な関数とほぼ同じこと", \t -> do
	itext t 1 "tryTrue :: Exception e =>"
	itext t 2 "(e -> Bool) -> IO a -> IO (Either e a)", \t -> do
	text t "* BoolではなくJust bとしている理由は", \t -> do
	itext t 1 "- Bool値をチェックするついでに", \t -> do
	itext t 1 "- 追加の処理も行っておくと効率的"
 ]

useTry6 :: Page
useTry6 = [\t -> do
	writeTopTitle t "同期例外の捕捉"
	text t "", \t -> do
	text t "* tryJustを使ってみる", \t -> do
	itext t 1 "> tryJust (guard . (== DivideByZero)) $"
	itext t 2 "print (1 `div` 0)"
	itext t 1 "Left ()", \t -> do
	itext t 1 "> tryJust (guard . (== DivideByZero)) $"
	itext t 2 "print (1 % 0)"
	itext t 1 "*** Exception: Ratio has zero denominator", \t -> do
	text t "* guardは第36回「functor, applicative, monad」で", \t -> do
	text t "* 今は以下のように考えておけば良い", \t -> do
	itext t 1 "guard True = Just ()"
	itext t 1 "guard _ = Nothing"
 ]

useTrySummary :: Page
useTrySummary = [\t -> do
	writeTopTitle t "同期例外の捕捉(まとめ)"
	text t "", \t -> do
	text t "* 同期例外の捕捉にはtryを使う", \t -> do
	itext t 1 "try :: Exception e => IO a -> IO (Either e e)", \t -> do
	text t "* 型eを決めてやる必要がある", \t -> do
	text t "* より細かく制御するためにはtryJustを使う", \t -> do
	itext t 1 "tryJust :: Exception e =>"
	itext t 2 "(e -> Maybe b) -> IO a -> IO (Either b a)", \t -> do
	text t "* (e -> Maybe b)型の関数で捕捉する例外をしぼり込む"
 ]

useCatch :: Page
useCatch = [\t -> do
	writeTopTitle t "非同期例外の捕捉"
	text t "", \t -> do
	text t "* 非同期例外の捕捉にはtryは使わない", \t -> do
	text t "* tryを使うと例外処理はmaskの外で行われることになる", \t -> do
	text t "* たとえば、", \t -> do
	itext t 1 "- getLine中にCtrl+Cを捕捉してretryするようなとき", \t -> do
	itext t 1 "- 例外処理がmaskの外にあると", \t -> do
	itext t 1 "- 例外処理中にCtrl+Cを受け取り例外終了する可能性", \t -> do
	text t "* 例外の捕捉からもれてしまう可能性がある", \t -> do
	text t "* catchを使う", \t -> do
	itext t 1 "catch :: Exception e =>"
	itext t 2 "IO a -> (e -> IO a) -> IO a"
 ]

useCatch2 :: Page
useCatch2 = [\t -> do
	writeTopTitle t "非同期例外の捕捉"
	text t "", \t -> do
	text t "* 例を見てみよう", \t -> do
	itext t 1 "> getLine `catch` \\(e :: AsyncException) -> "
	itext t 2 "print e >> return \"\"", \t -> do
	itext t 1 "Ctrl-C", \t -> do
	itext t 1 "user interrupt"
	itext t 1 "\"\""
 ]

useCatch3 :: Page
useCatch3 = [\t -> do
	writeTopTitle t "非同期例外の捕捉"
	text t "", \t -> do
	text t "* tryJustと同様のcatchJustがある", \t -> do
	itext t 1 "catchJust :: Exception e => (e -> Maybe b) ->"
	itext t 2 "IO a -> (b -> IO a) -> IO a", \t -> do
	text t "* これは仮想的なcatchTrueと同じこと", \t -> do
	itext t 1 "catchTrue :: Exception e => (e -> Bool) ->"
	itext t 2 "IO a -> (e -> IO a) -> IO a", \t -> do
	text t "* BoolではなくMaybe bにしているのは", \t -> do
	itext t 1 "- Bool値を求めるだけでなく追加の処理を行える"
 ]

useCatch4 :: Page
useCatch4 = [\t -> do
	writeTopTitle t "非同期例外の捕捉"
	text t "", \t -> do
	text t "* catchJustによる例外のしぼりこみ", \t -> do
	itext t 1 "> catchJust (guard . (== UserInterrupt)"
	itext t 3 ":: AsyncException -> Maybe ())"
	itext t 2 "getLine (const $ return \"\")", \t -> do
	itext t 1 "Ctrl-C", \t -> do
	itext t 1 "\"\""
 ]

useCatch5 :: Page
useCatch5 = [\t -> do
	writeTopTitle t "非同期例外の捕捉"
	text t "", \t -> do
	text t "* catch, catchJustの引数の順番を変えただけの関数がある", \t -> do
	itext t 1 "handle :: Exception e =>"
	itext t 2 "(e -> IO a) -> IO a -> IO a", \t -> do
	itext t 1 "handleJust :: Exception e => (e -> Maybe b) ->"
	itext t 2 "(b -> IO a) -> IO a -> IO a", \t -> do
	text t "* 以下のようなスタイルで使うと便利", \t -> do
	itext t 1 "handle someHandler $ do"
	itext t 2 "runSomeIO"
	itext t 2 "runNextIO"
 ]

useCatchSummary :: Page
useCatchSummary = [\t -> do
	writeTopTitle t "非同期例外の捕捉(まとめ)"
	text t "", \t -> do
	text t "* 非同期例外はcatchを使って捕捉する", \t -> do
	text t "* 例外処理は非同期例外がmaskされた状態で実行される", \t -> do
	text t "* 捕捉する例外をしぼりこむにはcatchJustを使う", \t -> do
	text t "* ScopedTypeVariables拡張を使うと書きやすい", \t -> do
	itext t 1 "catch getLine $ \\(e :: AsyncException) -> ...", \t -> do
	text t "* 場面合わせてhandle, handleJustを使う"
 ]

useCatches :: Page
useCatches = [\t -> do
	writeTopTitle t "複数種類の例外の捕捉"
	text t "", \t -> do
	text t "* 複数の種類の例外を捕捉したい場合", \t -> do
	itext t 0 "expr"
	preLine t
	itext t 1 "`catch` \\(ex :: ArithException) -> handleArith ex"
	itext t 1 "`catch` \\(ex :: IOException) -> handleIO ex", \t -> do
	text t "* しかし、これには問題がある", \t -> do
	itext t 1 "- 例外の捕捉を2度行っているため非効率", \t -> do
	itext t 1 "- ひとつめの例外処理中に生じた例外を"
	itext t 2 "ふたつめの例外処理が捕捉してしまう", \t -> do
	text t "* catches関数を使う", \t -> do
	itext t 1 "catches :: IO a -> [Handler a] -> IO a"
 ]

useCatches2 :: Page
useCatches2 = [\t -> do
	writeTopTitle t "複数種類の例外の捕捉"
	text t "", \t -> do
	text t "* Handler aとは何か?", \t -> do
	itext t 1 "data Handler a = forall e . Exception e =>"
	itext t 2 "Handler (e -> IO a)", \t -> do
	text t "* 複数の型をひとつの型にまとめる", \t -> do
	itext t 1 "- 存在型を使っている", \t -> do
	text t "* 以下のように使う", \t -> do
	itext t 0 "expr `catches` ["
	itext t 0.8 "Handler (\\(ex :: ArithException) -> handleArith ex,"
	itext t 0.8 "Handler (\\(ex :: IOException) -> handleIO ex ]"
 ]

useCatchesSummary :: Page
useCatchesSummary = [\t -> do
	writeTopTitle t "複数種類の例外の捕捉(まとめ)"
	text t "", \t -> do
	text t "* 複数の種類の例外を捕捉するにはcathesを使う", \t -> do
	text t "* catchesは例外処理関数のリストを取る", \t -> do
	text t "* 例外処理関数はそれぞれ型が異なる", \t -> do
	text t "* 型をまとめるためにHandlerを使う"
 ]

catchAll :: Page
catchAll = [\t -> do
	writeTopTitle t "すべての例外の捕捉"
	text t "", \t -> do
	text t "* すべての例外を捕捉しようと思ったら", \t -> do
	itext t 1 "- まずは「本当にそれが必要か」考えること", \t -> do
	text t "* すべての例外を捕捉することが不適切である例", \t -> do
	itext t 1 "- readFileでファイルがないときは\"\"を返したい", \t -> do
	itext t 1 "- こういうときに「すべての例外を捕捉」すると", \t -> do
	itext t 1 "- ユーザーがCtrl-Cで終了しようとしたとき", \t -> do
	itext t 1 "- 「空文字列を使い処理を続行」となってしまう", \t -> do
	text t "* すべての例外を捕捉する場面は以下の場合以外あまりない", \t -> do
	itext t 1 "- プログラムのトップレベルにおいて", \t -> do
	itext t 1 "- 例外をなんらかの形で記録または表示して", \t -> do
	itext t 1 "- 例外時のデフォルトの動作よりもおだやかに終了"
 ]

myException :: Page
myException = [\t -> do
	writeTopTitle t "自作の例外型を使う"
	text t "", \t -> do
	text t "* 例外型を自分で作ることができる"
 ]

catchAll2 :: Page
catchAll2 = [\t -> do
	writeTopTitle t "すべての例外の捕捉"
	text t "", \t -> do
	text t "* 例外は階層構造になっている", \t -> do
	text t "* 最上位にある型はSomeException", \t -> do
	text t "* SomeExceptionを使えばすべての例外が捕捉できる", \t -> do
	itext t 1 "runProgram `catch` \\(e :: SomeExpression) ->"
	itext t 2 "handleError e"
 ]

catchAllSummary :: Page
catchAllSummary = [\t -> do
	writeTopTitle t "すべての例外の捕捉(まとめ)"
	text t "", \t -> do
	text t "* できるだけ限定して例外を捕捉するべき", \t -> do
	text t "* 例外自体には興味がなく「どんなときも」行いたい後処理", \t -> do
	itext t 1 "- bracket, finally, onExceptionを使う", \t -> do
	text t "* すべての例外を捕捉するのは以下のときくらい", \t -> do
	itext t 1 "- プログラムをきれいに終了させたい", \t -> do
	text t "* その場合にはSomeException型を使えば良い"
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

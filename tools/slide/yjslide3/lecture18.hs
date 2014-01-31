import Lecture

subtitle :: String
subtitle = "第18回 IOモナド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], memo, memo2, memo3, memo4,
	prelude, prelude2,
	machine, machine2, machine3, machine4, machine5, machine6,
	machine7, machine7_1, machine8, machine8_1, machine9, machine9_1,
	machine10, machine10_1, machine10_2,
	machine11, machine11_2, machine11_3,
	machine12, machine12_1,
	machine13, machine14, machine15, machine15_1,
	machine16, machine17, machine17_1, machineSummary,
	aboutIO, aboutIO2, aboutIO3, aboutIO4, aboutIO5, aboutIO6
 ]

memo :: Page
memo = [\t -> do
	writeTopTitle t "業務連絡"
	text t "", \t -> do
	text t "* lectures/lecture18/IOMcn.hsを事前に作っておく", \t -> do
	itext t 0 "module IOMcn ("
	itext t 1 "IOMcn, runIOMcn, (>>>), arr, app,"
	itext t 1 "putLine, getLine, getInt, isEven) where"
	itext t 0 ""
	itext t 0 "import Prelude hiding (getLine)"
	itext t 0 "import qualified Prelude"
	itext t 0 "import Control.Arrow hiding ((>>>), arr)"
	itext t 0 "import qualified Control.Arrow"
	itext t 0 "import Control.Applicative"
	itext t 0 "import Data.Time"
 ]

memo2 :: Page
memo2 = [\t -> do
	writeTopTitle t "業務連絡"
	text t "", \t -> do
	itext t 0 "type IOMcn = Kleisli IO"
	itext t 0 ""
	itext t 0 "runIOMcn :: IOMcn () a -> IO a"
	itext t 0 "runIOMcn = (`runKleisli` ())"
	itext t 0 ""
	itext t 0 "(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c"
	itext t 0 "(>>>) = (Control.Arrow.>>>)"
	itext t 0 ""
	itext t 0 "arr :: (a -> b) -> IOMcn a b"
	itext t 0 "arr = Control.Arrow.arr"
 ]

memo3 :: Page
memo3 = [\t -> do
	writeTopTitle t "業務連絡"
	text t "", \t -> do
	itext t 0 "putLine :: IOMcn String ()"
	itext t 0 "putLine = Kleisli putStrLn"
	itext t 0 ""
	itext t 0 "getLine :: IOMcn () String"
	itext t 0 "getLine = Kleisli $ const Prelude.getLine"
 ]

memo4 :: Page
memo4 = [\t -> do
	writeTopTitle t "業務連絡"
	text t "", \t -> do
	itext t 0 "getInt :: IOMcn () Int"
	itext t 0 "getInt = Kleisli $ const $ Prelude.getLine >>= readIO"
	itext t 0 ""
	itext t 0 "isEven :: IOMcn () Bool"
	itext t 0 "isEven = Kleisli $ const $"
	itext t 1 "even . floor . utcDayTime <$> getCurrentTime"
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
--	text t "* Haskellは参照透過性を持つ言語である", \t -> do
--	text t "* つまり、関数の返す値は引数が同じなら常に同じ", \t -> do
--	text t "* また遅延評価する言語である", \t -> do
--	text t "* つまり、関数が評価されるタイミングが予測しづらい", \t -> do
	text t "* 多くの言語では以下のような形で入出力を扱う", \t -> do
	itext t 1 "- 関数が「評価」されるタイミングで入出力を行い", \t -> do
	itext t 1 "- 関数の返り値として入力を返す", \t -> do
	text t "* 関数の返り値として入力を返すと参照透過性が破壊される", \t -> do
	text t "* 「評価」のタイミングで入出力を行うと", \t -> do
	itext t 1 "- 「評価」の順がプログラムの意味に影響を与える"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* どうすればいいのだろう?", \t -> do
	itext t 1 "- 入出力を行う機械という値を作れば良い", \t -> do
	itext t 1 "- 数値型は対話環境で評価するとその値を表示する", \t -> do
	itext t 1 "- 文字型も対話環境で評価するとその値を表示する", \t -> do
	itext t 1 "- 機械型は対話環境で評価するとその動作を行う", \t -> do
	text t "* 機械型を数値型と同じように基本的な型として持てば良い"
 ]

machine :: Page
machine = [\t -> do
	writeTopTitle t "Machine"
	text t "", \t -> do
	text t "* たとえばMachine型という型があったとする", \t -> do
	text t "* Machine型の値としてputHelloとputWorldがあり", \t -> do
	itext t 1 "- それぞれ\"Hello\"と\"World\"を表示するとする", \t -> do
	itext t 1 "putHello :: Machine", \t -> do
	itext t 1 "putWorld :: Machine", \t -> do
	text t "* Machine型の値をつなぐ関数nextがあると", \t -> do
	itext t 1 "next :: Machine -> Machine -> Machine", \t -> do
	text t "* \"HelloWorld\"と表示する関数は以下のように書ける", \t -> do
	itext t 1 "putHelloWorld :: Machine", \t -> do
	itext t 1 "putHelloWorld = putHello `next` putWorld"
 ]

machine2 :: Page
machine2 = [\t -> do
	writeTopTitle t "Machine"
	text t "", \t -> do
	text t "* 出力についてはMachine型でうまくいく", \t -> do
	text t "* つまり「出力」というものは本質的に", \t -> do
	itext t 1 "- 「これやって」", \t -> do
	itext t 1 "- 「次にこれやって」", \t -> do
	text t "* それの連続なので。", \t -> do
	text t "* 入力がからむとこれはうまくいかなくなる", \t -> do
	text t "* 入力値を次の機械にわたす必要が出てくるからだ", \t -> do
	text t "* つまり機械のあいだで値をわたす仕組みが必要だ", \t -> do
	text t "* ひとつめの機械からふたつめの機械に値を渡す関数", \t -> do
	itext t 1 "(>>>) :: Machine -> Machine -> Machine"
 ]

machine3 :: Page
machine3 = [\t -> do
	writeTopTitle t "Machine"
	text t "", \t -> do
	text t "* 以下の機械があるとする", \t -> do
	itext t 1 "- getLine: 入力を一行読み、次の機械に渡す機械", \t -> do
	itext t 1 "- putLine: 渡された値を表示する機械", \t -> do
	text t "* 読み込んだ行を表示する機械は以下のように作れる", \t -> do
	itext t 1 "getLine >>> putLine"
 ]

machine4 :: Page
machine4 = [\t -> do
	writeTopTitle t "Machine"
	text t "", \t -> do
	text t "* しかしこのやりかたには問題がある", \t -> do
	text t "* 以下の機械があるとする", \t -> do
	itext t 1 "- getInt: 入力を読み、数に変換し次の機械に渡す機械", \t -> do
	text t "* そして次のようにすると", \t -> do
	itext t 1 "getInt >>> putLine", \t -> do
	text t "* putLineは文字列が来ることを期待しているので", \t -> do
	itext t 1 "- 数を渡されると予測出来ない動作をするだろう"
 ]

machine5 :: Page
machine5 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* つまり型の不一致が生じる可能性がある", \t -> do
	text t "* 静的型付け言語であるHaskellでは", \t -> do
	itext t 1 "- 型の不一致は型チェックの段階で検出したい", \t -> do
	text t "* Machine型に渡される値と渡す型の値の型を含めれば良い", \t -> do
	text t "* これをIOMcn型としよう", \t -> do
	itext t 1 "IOMcn a b"
 ]

machine6 :: Page
machine6 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 今まで出てきた機械の型は以下のようになる", \t -> do
	itext t 1 "putHello, putWorld :: IOMcn () ()", \t -> do
	itext t 1 "getLine :: IOMcn () String", \t -> do
	itext t 1 "getInt :: IOMcn () Int", \t -> do
	itext t 1 "putLine :: IOMcn String ()", \t -> do
	text t "* これらをつなぐ関数は以下のような型となる", \t -> do
	itext t 1 "(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c"
 ]

machine7 :: Page
machine7 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 例えば以下のようなつなぎかたは正当", \t -> do
	itext t 1 "getLine >>> putLine", \t -> do
	text t "* この場合それぞれの型は以下のようになる", \t -> do
	itext t 1 "getLine :: IOMcn () String", \t -> do
	itext t 1 "putLine :: IOMcn String ()", \t -> do
	itext t 1 "(>>>) :: IOMcn () String ->"
	itext t 2 "IOMcn String () -> IOMcn () ()", \t -> do
	text t "* つないだ結果の型は", \t -> do
	itext t 1 "getLine >>> putLine :: IOMcn () ()"
 ]

machine7_1 :: Page
machine7_1 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	text t "* lectures/lecture18/IOMcn.hsが用意してあるので", \t -> do
	itext t 1 "% ghci IOMcn.hs", \t -> do
	itext t 1 "*IOMcn> runIOMcn $ getLine >>> putLine", \t -> do
	itext t 2 "-- 何か適当に入力し改行する、ここでは\"hello\"", \t -> do
	itext t 1 "hello", \t -> do
	itext t 1 "hello", \t -> do
	text t "* 入力した文字列を表示している", \t -> do
	text t "* ここでは、runIOMcnで機械を動かしていると考える"
 ]

machine8 :: Page
machine8 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* しかし、以下のつなぎかたは型の不適合となる", \t -> do
	itext t 1 "getInt >>> putLine", \t -> do
	text t "* (>>>)の型を再掲する", \t -> do
	itext t 1 "(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c", \t -> do
	text t "* (>>>)のbの型が", \t -> do
	itext t 1 "- getIntからはIntであることを要求され", \t -> do
	itext t 1 "- putLineからはStringであることを要求される", \t -> do
	text t "* 結果として型エラーとなる"
	text t "", \t -> do
	arrowIText t 1 "おかしな型の値が機械に渡されることはない"
 ]

machine8_1 :: Page
machine8_1 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*IOMcn> runIOMcn $ getInt >>> putLine", \t -> do
	itext t 1 "..."
	itext t 1 "Couldn't match type `[Char]' with `Int'"
	itext t 1 "...", \t -> do
	text t "* 確かに、型エラーとなる"
 ]

machine9 :: Page
machine9 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* (>>>)を使えば次々と機械をつないでいくことができる", \t -> do
	itext t 1 "m1 >>> m2 >>> m3 >>> m4 >>> ...", \t -> do
	text t "* 途中に普通の関数をはさみたいこともある", \t -> do
	itext t 1 "- 入力された文字列を逆にして表示したい等々", \t -> do
	text t "* 関数を機械に変換する関数が必要になる", \t -> do
	itext t 1 "arr :: (a -> b) -> IOMcn a b", \t -> do
	text t "* これを使うと入力を逆順にして表示は", \t -> do
	itext t 1 "getLine >>> arr reverse >>> putLine", \t -> do
	text t "* arr reverseは", \t -> do
	itext t 1 "- 文字列を受け取り", \t -> do
	itext t 1 "- それを逆順にして次の機械に渡す機械"
 ]

machine9_1 :: Page
machine9_1 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 0 "*IOMcn> runIOMcn $ getLine >>> arr reverse >>> putLine", \t -> do
	itext t 1 "-- \"hello\"を入力してみよう", \t -> do
	itext t 0 "hello", \t -> do
	itext t 0 "olleh", \t -> do
	text t "* 入力された文字列が逆順で表示された"
 ]

machine10 :: Page
machine10 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 関数がIOMcnに変換できるということは", \t -> do
	itext t 1 "- 普通の値を機械に流し込むことができるということ", \t -> do
	text t "* たとえば\"Hello\"を機械に流し込むには以下のようにする", \t -> do
	itext t 1 "arr (const \"Hello\") >>> putLine", \t -> do
	text t "* 引き数を無視し\"Hello\"を返す関数を機械に変換し", \t -> do
	itext t 1 "- その機械と機械putLineとをつないだ", \t -> do
	text t "* つまりputHello, putWorldは以下のように定義できる", \t -> do
	itext t 1 "putHello = arr (const \"Hello\") >>> putLine", \t -> do
	itext t 1 "putWorld = arr (const \"World\") >>> putLine"
 ]

machine10_1 :: Page
machine10_1 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*IOMcn> runIOMcn $ arr (const \"Hello\") >>> putLine", \t -> do
	itext t 1 "Hello", \t -> do
	itext t 1 "*IOMcn> runIOMcn $ arr (const \"World\") >>> putLine", \t -> do
	itext t 1 "World", \t -> do
	text t "* 引数を無視して\"Hello\"を返す関数(const \"Hello\")", \t -> do
	text t "* これを機械に変換", \t -> do
	itext t 1 "arr (const \"Hello\") :: IOMcn () String", \t -> do
	text t "* これをputLineにつないでいる"
 ]

machine10_2 :: Page
machine10_2 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* hello.hsを作って以下を書き込もう", \t -> do
	itext t 1 "import IOMcn", \t -> do
	itext t 1 "putHello, putWorld :: IOMcn () ()", \t -> do
	itext t 1 "putHello = arr (const \"Hello\") >>> putLine", \t -> do
	itext t 1 "putWorld = arr (const \"World\") >>> putLine", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "*IOMcn> :load hello.hs", \t -> do
	itext t 1 "*Main> runIOMcn $ putHello >>> putWorld", \t -> do
	itext t 1 "Hello"
	itext t 1 "World"
 ]

machine11 :: Page
machine11 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 以下のようなあいさつをする機械が作りたい", \t -> do
	itext t 1 "- 偶数の秒には\"olleh\"", \t -> do
	itext t 1 "- 奇数の秒には\"hello\"", \t -> do
	text t "* 今が偶数の秒かどうかを返す機械はあると考えよう", \t -> do
	itext t 0.8 "isEven :: IOMcn () Bool", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> runIOMcn isEven", \t -> do
	itext t 2 "-- その時によってTrueかFalseが表示される", \t -> do
	itext t 1 "True", \t -> do
	itext t 1 "*Main> runIOMcn isEven", \t -> do
	itext t 1 "False"
 ]

machine11_2 :: Page
machine11_2 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* Bool値によって以下のどちらかを返す関数を作ろう", \t -> do
	itext t 1 "- メッセージを逆順で表示する機械", \t -> do
	itext t 1 "- メッセージをそのまま表示する機械", \t -> do
	text t "* その関数をmessageという名前で定義する", \t -> do
	itext t 1 "message :: Bool -> IOMcn String ()", \t -> do
	itext t 1 "message True = arr reverse >>> putLine", \t -> do
	itext t 1 "message False = putLine", \t -> do
	text t "* これを以下と併わせてgreeting.hsに書き込もう", \t -> do
	itext t 1 "import IOMcn"
 ]

machine11_3 :: Page
machine11_3 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 0 "*Main> :load greeting.hs", \t -> do
	itext t 0 "*Main> runIOMcn $ arr (const \"hello\") >>> message False", \t -> do
	itext t 0 "hello", \t -> do
	itext t 0 "*Main> runIOMcn $ arr (const \"hello\") >>> message True", \t -> do
	itext t 0 "olleh"
 ]

machine12 :: Page
machine12 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 以下の機械と関数がある", \t -> do
	itext t 1 "isEven :: IOMcn () Bool", \t -> do
	itext t 1 "message :: Bool -> IOMcn String ()", \t -> do
	text t "* arrと>>>を使って組み合わせて以下の動作の機械を作る", \t -> do
	itext t 1 "- 偶数の秒には\"hello\"を逆順で表示し", \t -> do
	itext t 1 "- 奇数の秒には\"hello\"を表示する", \t -> do
	text t "* 渡されたBool値を受け取るにはmessage関数を機械にする", \t -> do
	itext t 1 "arr message :: IOMcn Bool (IOMcn String ())", \t -> do
	text t "* これとisEvenをつなげると", \t -> do
	itext t 1 "isEven >>> arr message ::"
	itext t 2 "IOMcn () (IOMcn String ())"
 ]

machine12_1 :: Page
machine12_1 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 実際に型を見てみよう", \t -> do
	itext t 0 "*Main> :t message", \t -> do
	itext t 0 "message :: Bool -> IOMcn String ()", \t -> do
	itext t 0 "*Main> :t arr message", \t -> do
	itext t 0 "arr message :: IOMcn Bool (IOMcn String ())", \t -> do
	itext t 0 "*Main> :t isEven >>> arr message", \t -> do
	itext t 0 "isEven >>> arr message :: IOMcn () (IOMcn String ())"
 ]

machine13 :: Page
machine13 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* isEven >>> arr messageの型を見ると", \t -> do
	itext t 1 "IOMcn () (IOMcn String ())", \t -> do
	text t "* 機械を渡す機械ができてしまっている", \t -> do
	text t "* IOMcn String ()を動かすにはStringを渡す必要がある", \t -> do
	text t "* IOMcn String ()にStringを渡す機械が必要", \t -> do
	text t "* より一般的にはIOMcn a bにaを渡す機械が必要になる", \t -> do
	itext t 1 "app :: IOMcn (IOMcn a b, a) b", \t -> do
	text t "* (「aを受け取りbを渡す機械」とa)を受け取りbを渡す機械", \t -> do
	text t "* appを使うには以下の型の機械を作る必要がある", \t -> do
	itext t 1 "IOMcn () (IOMcn String (), String)"
 ]

machine14 :: Page
machine14 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 以下の型の機械と関数がある", \t -> do
	itext t 1 "isEven :: IOMcn () Bool", \t -> do
	itext t 1 "message :: Bool -> IOMcn String ()", \t -> do
	text t "* 組み合わせるための道具には以下のものがある", \t -> do
	itext t 1 "arr :: (a -> b) -> IOMcn a b", \t -> do
	itext t 1 "(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c", \t -> do
	itext t 1 "app :: IOMcn (IOMcn a b, a) b", \t -> do
	text t "* 今、作りたい機械の型は", \t -> do
	itext t 1 "IOMcn () (IOMcn String (), String)", \t -> do
	text t "* この型の関数は以下の型の関数とisEvenをつなげばできる", \t -> do
	itext t 1 "IOMcn Bool (IOMcn String (), String)"
 ]

machine15 :: Page
machine15 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* この型の関数を作るには"
	itext t 1 "IOMcn Bool (IOMcn String (), String)", \t -> do
	text t "* 以下の型の関数にarrを適用すれば良い", \t -> do
	itext t 1 "Bool -> (IOMcn String (), String)", \t -> do
	text t "* messageを使えばこの型の関数はすぐに作れる", \t -> do
	itext t 1 "sayHello :: Bool -> (IOMcn String (), String)", \t -> do
	itext t 1 "sayHello b = (message b, \"hello\")", \t -> do
	text t "* 関数sayHelloをgreeting.hsに書き込もう"
 ]

machine15_1 :: Page
machine15_1 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 対話環境で求める機械を組み立ててみる", \t -> do
	itext t 0 "*Main> :reload", \t -> do
	itext t 0 "*Main> :t arr sayHello", \t -> do
	itext t 0 "arr sayHello :: IOMcn Bool (IOMcn String (), String)", \t -> do
	itext t 0 "*Main> :t isEven >>> arr sayHello", \t -> do
	itext t 0 "isEven >>> arr sayHello ::"
	itext t 1 "IOMcn () (IOMcn String (), String)", \t -> do
	itext t 0 "*Main> :t isEven >>> arr sayHello >>> app", \t -> do
	itext t 0 "isEven >>> arr sayHello >>> app :: IOMcn () ()", \t -> do
	itext t 0 "*Main> runIOMcn $ isEven >>> arr sayHello >>> app", \t -> do
	itext t 0 "olleh"
	itext t 1 "-- 時間によってhelloまたはolleh"
 ]

machine16 :: Page
machine16 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* まとめると以下のようになる", \t -> do
	itext t 0 "sayHello :: Bool -> (IOMcn String (), String)", \t -> do
	itext t 0 "arr sayHello :: IOMcn Bool (IOMcn String (), String)", \t -> do
	itext t 0 "isEven >>> arr sayHello ::"
	itext t 1 "IOMcn () (IOMcn String (), String)", \t -> do
	itext t 0 "isEven >>> arr sayHello >>> app :: IOMcn () ()"
 ]

machine17 :: Page
machine17 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 求める関数greetingは", \t -> do
	itext t 1 "greeting :: IOMcn () ()", \t -> do
	itext t 1 "greeting = isEven >>> arr sayHello >>> app", \t -> do
	text t "* それぞれの機械を説明すると", \t -> do
	itext t 1 "- Boolを渡す機械", \t -> do
	itext t 1 "- Boolを受け取り"
	itext t 2 "(「文字列を受け取る機械」と文字列)を渡す機械", \t -> do
	itext t 1 "- (「文字列を受け取る機械M」と文字列S)を受け取り"
	itext t 2 "機械Mに文字列Sを渡す機械", \t -> do
	text t "* 関数greetingの定義をgreeting.hsに書き込もう"
 ]

machine17_1 :: Page
machine17_1 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> runIOMcn greeting", \t -> do
	itext t 1 "hello", \t -> do
	itext t 1 "*Main> runIOMcn greeting", \t -> do
	itext t 1 "hello", \t -> do
	itext t 1 "*Main> runIOMcn greeting", \t -> do
	itext t 1 "olleh"
 ]

machineSummary :: Page
machineSummary = [\t -> do
	writeTopTitle t "IOMcn(まとめ)"
	text t "", \t -> do
	text t "* 多くの言語ではIOは以下のように行われる", \t -> do
	itext t 1 "- 関数の評価のタイミングで入出力動作を行い", \t -> do
	itext t 1 "- 入力値は関数の返り値として受け取れる", \t -> do
	text t "* 参照透過性と遅延評価の面から上記の方法は望ましくない", \t -> do
	text t "* むしろIOを行う機械を組み立てていくことを考える", \t -> do
	text t "* 機械の受け取りの型と受け渡しの型を指定すると良い", \t -> do
	text t "* 以下の型とそれに対する関数や機械を用意しておくと良い", \t -> do
	itext t 1 "IOMcn a b", \t -> do
	itext t 1 "(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c", \t -> do
	itext t 1 "arr :: (a -> b) -> IOMcn a b", \t -> do
	itext t 1 "app :: IOMcn (IOMcn a b, a) b"
 ]

aboutIO :: Page
aboutIO = [\t -> do
	writeTopTitle t "IO"
	text t "", \t -> do
	text t "* IOMcnはもっとスマートにすることができる", \t -> do
	text t "* 以下の型を比較してみる", \t -> do
	itext t 1 "IOMcn a b", \t -> do
	itext t 1 "a -> IOMcn () b", \t -> do
	text t "* これらの型が相互に変換可能であることを示そう"
 ]

aboutIO2 :: Page
aboutIO2 = [\t -> do
	writeTopTitle t "IO"
	text t "", \t -> do
	text t "* まずは(IOMcn a b)から(a -> IOMcn () b)を作る関数", \t -> do
	itext t 1 "outArg :: IOMcn a b -> a -> IOMcn () b", \t -> do
	itext t 1 "outArg iom = \\x -> arr (const x) >>> iom", \t -> do
	text t "* 型aの値xがあれば以下の機械が作れる", \t -> do
	itext t 1 "arr (const x) :: IOMcn () a", \t -> do
	text t "* これと(iom :: IOMcn a b)をつなげればIOMcn () bは作れる", \t -> do
	text t "* 「(IOMcn a b)とaからIOMcn () bを作れる」は以下と同値", \t -> do
	itext t 1 "- 「(IOMcn a b)から(a -> IOMcn () b)を作れる」"
 ]

aboutIO3 :: Page
aboutIO3 = [\t -> do
	writeTopTitle t "IO"
	text t "", \t -> do
	text t "* 逆に(a -> IOMcn () b)から(IOMcn a b)が作れる", \t -> do
	itext t 1 "inArg :: (a -> IOMcn () b) -> IOMcn a b", \t -> do
	itext t 1 "inArg f = arr (\\x -> (f x, ())) >>> app", \t -> do
	text t "* (a -> IOMcn () b)があれば以下の関数が作れる", \t -> do
	itext t 1 "\\x -> (f x, ()) :: a -> (IOMcn () b, ())", \t -> do
	text t "* ここから以下の機械が作れる", \t -> do
	itext t 1 "arr (\\x -> (f x, ())) :: IOMcn a (IOMcn () b, ())", \t -> do
	text t "* さらに機械appをつなげば良い", \t -> do
	itext t 1 "arr (\\x -> (f x, ())) >>> app :: IOMcn a b"
 ]

aboutIO4 :: Page
aboutIO4 = [\t -> do
	writeTopTitle t "IO"
	text t "", \t -> do
	text t "* つまり以下の2つの型は同じものであると考えられる", \t -> do
	itext t 1 "IOMcn a b", \t -> do
	itext t 1 "a -> IOMcn () b", \t -> do
	text t "* aを受け取ってbを渡す機械を", \t -> do
	itext t 1 "- aの値によって「bを渡す機械」を選ぶ関数に変換可能", \t -> do
	text t "* IOMcn a bの形の関数をa -> IOMcn () bの形に統一し", \t -> do
	itext t 1 "type IO = IOMcn ()としてみよう"
 ]

aboutIO5 :: Page
aboutIO5 = [\t -> do
	writeTopTitle t "IO"
	text t "", \t -> do
	text t "* すると以下の関数のペアを", \t -> do
	itext t 1 "(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c", \t -> do
	itext t 1 "arr :: (a -> b) -> IOMcn a b", \t -> do
	text t "* 以下の形に変えることができる", \t -> do
	itext t 1 "(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)", \t -> do
	itext t 1 "arr' :: (a -> b) -> (a -> IO b)", \t -> do
	text t "* これはモナド関数だ", \t -> do
	itext t 1 "(>>=) :: IO a -> (a -> IO b) -> IO b", \t -> do
	itext t 1 "return :: a -> IO a"
 ]

aboutIO6 :: Page
aboutIO6 = [\t -> do
	writeTopTitle t "IO"
	text t "", \t -> do
	text t "* aを受け取りbを渡す機械を以下の関数に変換する", \t -> do
	itext t 1 "- aを引数として取り「bを渡す機械」を返す関数", \t -> do
	text t "* そうすることによりIOを行う機械をモナドとして扱える", \t -> do
	text t "* 文字列を表示する関数", \t -> do
	itext t 1 "putStrLn :: String -> IO ()", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> putStrLn \"Hello\"", \t -> do
	itext t 1 "Hello"
 ]

import Lecture

subtitle :: String
subtitle = "第18回 IOモナド"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	machine, machine2, machine3, machine4, machine5, machine6,
	machine7, machine8, machine9, machine10, machine11, machine12
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellは参照透過性を持つ言語である", \t -> do
	text t "* つまり、関数の返す値は引数が同じなら常に同じ", \t -> do
	text t "* また遅延評価する言語である", \t -> do
	text t "* つまり、関数が評価されるタイミングが予測しづらい", \t -> do
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
	text t "* ひとつめの機械からふたつめの機械に値をさたす関数", \t -> do
	itext t 1 "pipe :: Machine -> Machine -> Machine"
 ]

machine3 :: Page
machine3 = [\t -> do
	writeTopTitle t "Machine"
	text t "", \t -> do
	text t "* 以下の機械があるとする", \t -> do
	itext t 1 "- getLine: 入力を一行読み、次の機械に渡す機械", \t -> do
	itext t 1 "- putLine: 渡された値を表示する機械", \t -> do
	text t "* 読み込んだ行を表示する機械は以下のように作れる", \t -> do
	itext t 1 "getLine `pipe` putLine"
 ]

machine4 :: Page
machine4 = [\t -> do
	writeTopTitle t "Machine"
	text t "", \t -> do
	text t "* しかしこのやりかたには問題がある", \t -> do
	text t "* 以下の機械があるとする", \t -> do
	itext t 1 "- getInt: 入力を読み、数に変換し次の機械に渡す機械", \t -> do
	text t "* そして次のようにすると", \t -> do
	itext t 1 "getInt `pipe` putLine", \t -> do
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
	itext t 1 "pipe :: IOMcn a b -> IOMcn b c -> IOMcn a c"
 ]

machine7 :: Page
machine7 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 例えば以下のようなつなぎかたは正当", \t -> do
	itext t 1 "getLine `pipe` putLine", \t -> do
	text t "* この場合それぞれの型は以下のようになる", \t -> do
	itext t 1 "getLine :: IOMcn () String", \t -> do
	itext t 1 "putLine :: IOMcn String ()", \t -> do
	itext t 1 "pipe :: IOMcn () String ->"
	itext t 2 "IOMcn String () -> IOMcn () ()", \t -> do
	text t "* つないだ結果の型は", \t -> do
	itext t 1 "getLIne `pipe` putLine :: IOMcn () ()"
 ]

machine8 :: Page
machine8 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* しかし、以下のつなぎかたは型の不適合となる", \t -> do
	itext t 1 "getInt `pipe` putLine", \t -> do
	text t "* pipeの型を再掲する", \t -> do
	itext t 1 "pipe :: IOMcn a b -> IOMcn b c -> IOMcn a c", \t -> do
	text t "* pipeのbの型が", \t -> do
	itext t 1 "- getIntからはIntであることを要求され", \t -> do
	itext t 1 "- putLineからはStringであることを要求される", \t -> do
	text t "* 結果として型エラーとなる"
	text t "", \t -> do
	arrowIText t 1 "おかしな型の値が機械に渡されることはない"
 ]

machine9 :: Page
machine9 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* pipeを使えば次々と機械をつないでいくことができる", \t -> do
	itext t 1 "m1 `pipe` m2 `pipe` m3 `pipe` m4 `pipe` ...", \t -> do
	text t "* 途中に普通の関数をはさみたいこともある", \t -> do
	itext t 1 "- 入力された文字列を逆にして表示したい等々", \t -> do
	text t "* 関数を機械に変換する関数が必要になる", \t -> do
	itext t 1 "arrM :: (a -> b) -> IOMcn a b", \t -> do
	text t "* これを使うと入力を逆順にして表示は", \t -> do
	itext t 1 "getLine `pipe` arrM reverse `pipe` putLine"
 ]

machine10 :: Page
machine10 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 関数がIOMcnに変換できるということは", \t -> do
	itext t 1 "- 普通の値を機械に流し込むことができるということ", \t -> do
	text t "* たとえば\"Hello\"を機械に流し込むには以下のようにする", \t -> do
	itext t 1 "arrM (const \"Hello\") `pipe` putLine", \t -> do
	text t "* 引き数を無視し\"Hello\"を返す関数を機械に変換し", \t -> do
	itext t 1 "- その機械と機械putLineとをつないだ", \t -> do
	text t "* つまりputHello, putWorldは以下のように定義できる", \t -> do
	itext t 1 "putHello = arrM (const \"Hello\") `pipe` putLine", \t -> do
	itext t 1 "putWorld = arrM (const \"World\") `pipe` putLine"
 ]

machine11 :: Page
machine11 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 偶数の日には文字を逆から読みたい人がいたとする", \t -> do
	text t "* 今日が偶数の日かどうかを返す機械はあると考えよう", \t -> do
	itext t 0.8 "isEven :: IOMcn () Bool", \t -> do
	text t "* Bool値によって以下のどちらかを返す関数", \t -> do
	itext t 1 "- メッセージをそのまま表示する機械", \t -> do
	itext t 1 "- メッセージを逆順で表示する機械", \t -> do
	text t "* その関数をmessageという名前で定義する", \t -> do
	itext t 1 "message :: Bool -> IOMcn String ()", \t -> do
	itext t 1 "message True = arrM reverse `pipe` putLine", \t -> do
	itext t 1 "message False = putLine"

 ]

machine12 :: Page
machine12 = [\t -> do
	writeTopTitle t "IOMcn"
	text t "", \t -> do
	text t "* 以下の機械と関数がある", \t -> do
	itext t 1 "isEven :: IOMcn () Bool", \t -> do
	itext t 1 "message :: Bool -> IOMcn String ()", \t -> do
	text t "* arrMとpipeを使って組み合わせて以下の動作の機械を作る", \t -> do
	itext t 1 "- 偶数の日には渡された文字列を逆順で表示する", \t -> do
	text t "* 渡されたBool値を受け取るにはmessage関数を機械にする", \t -> do
	itext t 1 "arrM message :: IOMcn Bool (IOMcn String ())", \t -> do
	text t "* これとisEvenをつなげると", \t -> do
	itext t 1 "isEven `pipe` arrM message ::"
	itext t 2 "IOMcn () (IOMcn String ())"
 ]

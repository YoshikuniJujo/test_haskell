import Lecture

subtitle :: String
subtitle = "チューリングマシンとラムダ計算"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	[flip writeTitle subtitle], turingMachine, turingMachine2,
	lambdaCalculus, turingAndLambda, factorial,
	proceduralAndFunctional, proceduralAndFunctional2,
	history, state, stateInFun, stateInObject
 ]

turingMachine :: Page
turingMachine = [\t -> do
	writeTopTitle t "チューリングマシンとは"
	text t "", \t -> do
	text t "* 無限に長いテープとヘッダからなる機械", \t -> do
	text t "* ヘッダには動作のルールが書き込まれている", \t -> do
	text t "* ヘッダは状態を持つ", \t -> do
	text t "* テープの内容を読み込む", \t -> do
	text t "* 状態と内容から適用されるルールが決まる", \t -> do
	text t "* そのルールにしたがってテープの内容を書き換え", \t -> do
	text t "* 状態を変化させて", \t -> do
	text t "* テープの左右どちらかにヘッダを動かす"
 ]

turingMachine2 :: Page
turingMachine2 = [\t -> do
	writeTopTitle t "ノイマン型コンピュータとは"
	text t "", \t -> do
	text t "* チューリングマシンと本質的には似ている", \t -> do
	text t "* メモリから命令を読み込み", \t -> do
	text t "* それに応じて以下の動作を行う", \t -> do
	itext t 1 "- メモリからレジスタへ値を読み込む", \t -> do
	itext t 1 "- レジスタからメモリへ値を書き出す", \t -> do
	itext t 1 "- レジスタ内の値を変化させる", \t -> do
	itext t 1 "- 命令を読み込むメモリ番地を変化させる"
 ]

lambdaCalculus :: Page
lambdaCalculus = [\t -> do
	writeTopTitle t "ラムダ計算とは"
	text t "", \t -> do
	text t "* 関数の適用によって計算を行う", \t -> do
	text t "* f(x) = x * xのような関数fを", \t -> do
	itext t 1 "- λx.x*xのように表現する", \t -> do
	itext t 1 "- これは関数のリテラル表記と言える", \t -> do
	itext t 1 "- Haskellの記法では\\x -> x * xと書く", \t -> do
	text t "* 関数の適用は以下のようになる", \t -> do
	itext t 1 "(\\x -> x * x) 3", \t -> do
	itext t 1 "-> 3 * 3", \t -> do
	itext t 1 "-> 9"
 ]

turingAndLambda :: Page
turingAndLambda = [\t -> do
	writeTopTitle t "チューリングマシンとラムダ計算"
	text t "", \t -> do
	text t "* チューリングマシンが手続き型であり", \t -> do
	text t "* ラムダ計算が関数型であると言える", \t -> do
	text t "* チューリングマシンとラムダ計算ではできることは同じ"
 ]

factorial :: Page
factorial = [\t -> do
	writeTopTitle t "階乗の計算"
	text t "", \t -> do
	text t "* xの階乗の計算を手続き型で表現するとこうなる", \t -> do
	itext t 1 "1. nにxを代入する", \t -> do
	itext t 1 "2. rに1を代入する", \t -> do
	itext t 1 "3. nが1以上のあいだ4から5をくりかえす", \t -> do
	itext t 1 "4. rにnをかけたものをrに代入する", \t -> do
	itext t 1 "5. nから1を引いたものをnに代入する", \t -> do
	itext t 1 "6. rの値が結果となる", \t -> do
	text t "* xの階乗を関数型で表現するとこうなる", \t -> do
	itext t 1 "1. 0の階乗は1である", \t -> do
	itext t 1 "2. nの階乗はnかける(n-1の階乗)である"
 ]

proceduralAndFunctional :: Page
proceduralAndFunctional = [\t -> do
	writeTopTitle t "手続き型と関数型"
	text t "", \t -> do
	text t "* 手続き型では「何をするか」を記述している", \t -> do
	text t "* 関数型では「何であるか」を記述している", \t -> do
	text t "* 物事が単純なうちは手続き型のほうが簡単", \t -> do
	text t "* 物事が複雑になると関数型でないと収拾がつかなくなる"
 ]

proceduralAndFunctional2 :: Page
proceduralAndFunctional2 = [\t -> do
	writeTopTitle t "手続き型と関数型"
	text t "", \t -> do
	text t "* 現在多くの言語は「手続き型」と呼ばれている、が", \t -> do
	text t "* 「純粋手続き型言語」と言えるのは", \t -> do
	itext t 1 "- 機械語", \t -> do
	itext t 1 "- アセンブリ言語", \t -> do
	itext t 1 "- BASIC", \t -> do
	text t "* これらは関数型の特徴を持たない純粋な手続き型言語", \t -> do
	text t "* 多くの言語は「手続き型」と「関数型」のハイブリッド", \t -> do
	text t "* 一般的にはそのなかで", \t -> do
	itext t 1 "- 「手続き型」の特徴が強いものを手続き型言語とし", \t -> do
	itext t 1 "- 「関数型」の特徴が強いものを関数型言語とする"
 ]

history :: Page
history = [\t -> do
	writeTopTitle t "言語の進化の歴史"
	text t "", \t -> do
	text t "* 言語の進化の歴史は", \t -> do
	itext t 1 "- 手続き型から関数型への進化と考えることができる", \t -> do
	text t "* 図式的な言いかたをすれば", \t -> do
	itext t 1 "- チューリングマシン上でのラムダ計算のエミュレート", \t -> do
	text t "* それを意識的に行ったのはLispである、が", \t -> do
	text t "* C言語においてさえ、それは生じている"
 ]

state :: Page
state = [\t -> do
	writeTopTitle t "状態"
	text t "", \t -> do
	text t "* 本当の「関数」には状態は無い", \t -> do
	text t "* 「関数」は本来は入力と出力のあいだの関係を表す", \t -> do
	text t "* 同じ入力に対しては同じ出力でなければならない", \t -> do
	text t "* 時によって4の2倍の値が変化したら変だ", \t -> do
	text t "* Cの「関数」は本来の関数としての性質だけでなく", \t -> do
	itext t 1 "- 手続きをまとめた台本としての性質を持つ", \t -> do
	text t "* Cの「関数」は呼び出しのたびに違う値を返す可能性がある"
 ]

stateInFun :: Page
stateInFun = [\t -> do
	writeTopTitle t "状態"
	text t "", \t -> do
	text t "* Cでは関数のなかで状態変化させてしまうため", \t -> do
	text t "* counter()という関数呼び出しは", \t -> do
	itext t 1 "- ひとつのプログラムのなかでひとつしか使えない", \t -> do
	text t "* 2つの別のものをcountすようとすると", \t -> do
	itext t 1 "- 競合しておかしなことになってしまう", \t -> do
	text t "* そこで状態を関数内部ではなく、", \t -> do
	itext t 1 "- 特別扱いされる第一引数に移した", \t -> do
	arrowIText t 1 "オブジェクト指向"
 ]

stateInObject :: Page
stateInObject = [\t -> do
	writeTopTitle t "状態"
	text t "", \t -> do
	text t "* オブジェクト指向では状態変化は", \t -> do
	itext t 1 "- 関数からオブジェクトに移行された", \t -> do
	text t "* Haskellでは状態変化は言語の外にまで追いやられた"
 ]

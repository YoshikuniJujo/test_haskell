import Lecture

subtitle :: String
subtitle = "1. 関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], repl1, repl2, loadFile,
	function0,
	function1, function2, function3, function4, function5, function6,
	higher1,
	operator1, operator2,
	summary
 ]

repl1 :: Page
repl1 = [ \t -> do
	writeTopTitle t "対話環境(1)"
	text t "", \t -> do
	text t "* ghcが適切にインストールされていることを確認する", \t -> do
	itext t 1 "% ghc --version", \t -> do
	itext t 1 "The Glorious Glasgow Haskell Compilatio System,"
	itext t 1 "version X.Y.Z", \t -> do
	text t "* 対話環境の開始", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> ", \t -> do
	text t "* 終了", \t -> do
	itext t 1 "Prelude> :quit"
	]

repl2 :: Page
repl2 = [ \t -> do
	writeTopTitle t "対話環境(2)"
	text t "", \t -> do
	text t "* 値を入力してみよう", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "Prelude> 4492", \t -> do
	itext t 1 "4492", \t -> do
	itext t 1 "Prelude> 'c'", \t -> do
	itext t 1 "'c'", \t -> do
	itext t 1 "Prelude> pi", \t -> do
	itext t 1 "3.141592653589793"
	]

loadFile :: Page
loadFile = [ \t -> do
	writeTopTitle t "ファイルの読みこみ"
	text t "", \t -> do
	text t "* 以下のようなfruits.hsを作成"
	itext t 1 "myFavoriteFruit = \"apple\"", \t -> do
	text t "* 対話環境に読みこみ確認する", \t -> do
	itext t 1 "% ghci fruits.hs", \t -> do
	itext t 1 "*Main> myFavoriteFruit", \t -> do
	itext t 1 "\"apple\"", \t -> do
	text t "* 対話環境は走らせたままファイルを編集"
	itext t 1 "myFavoriteFruit = \"banana\"", \t -> do
	text t "* ファイルを再読みこみし確認する", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> myFavoriteFruit", \t -> do
	itext t 1 "\"banana\""
	]

function0 :: Page
function0 = [ \t -> do
	writeTopTitle t "関数"
	text t "", \t -> do
	text t "* 他の言語の知識があると誤解しがちだが", \t -> do
	text t "* 関数とは「手続きをまとめたもの」ではない", \t -> do
	text t "* 関数とは「値を別の値に関連づける規則」だ", \t -> do
	text t "* 「2倍する」は「関数」だ", \t -> do
	itext t 1 "1を2に、2を4に、3を6に関連づける", \t -> do
	text t "* 「ファイルの内容を表示する」は関数ではない", \t -> do
	text t "* 「2倍する」関数を書いてみよう", \t -> do
	itext t 1 "% vi functions.hs", \t -> do
	itext t 1 "double x = 2 * x", \t -> do
	itext t 1 "% ghci functions.hs", \t -> do
	itext t 1 "*Main> double 4492", \t -> do
	itext t 1 "8984"
	]

function1 :: Page
function1 = [ \t -> do
	writeTopTitle t "関数の構文"
	text t "", \t -> do
	text t "* 関数の定義は簡単だ", \t -> do
	itext t 1 "[関数名] [引数] = [式]", \t -> do
	text t "* 関数doubleでは", \t -> do
	itext t 1 "double x = 2 * x"
	]

function2 :: Page
function2 = [ \t -> do
	writeTopTitle t "関数リテラル"
	text t "", \t -> do
	text t "* 関数doubleの定義は以下のようだった", \t -> do
	itext t 1 "double x = 2 * x", \t -> do
	text t "* この定義は実は2つのことをしている", \t -> do
	itext t 1 "1. 引数を2倍する関数を作って", \t -> do
	itext t 1 "2. その関数にdoubleという名前をつける", \t -> do
	text t "* これをちゃんと書くと", \t -> do
	itext t 1 "double = \\x -> 2 * x", \t -> do
	text t "* 名前をつけなくても使える", \t -> do
	itext t 1 "*Main> (\\x -> 2 * x) 4492", \t -> do
	itext t 1 "8984"
	]

function3 :: Page
function3 = [ \t -> do
	writeTopTitle t "多引数関数"
	text t "", \t -> do
	text t "* 関数とは「ひとつの値をひとつの値に関連づけるもの」だ", \t -> do
	text t "* ふたつの引数が必要になったらどうすればいいか", \t -> do
	text t "* ひとつの引数をとって", \t -> do
	itext t 1 "「ひとつの引数をとって値を返す関数」を返せば良い", \t -> do
	text t "* BMIは体重を身長の自乗でわったものだ", \t -> do
	text t "* BMIを求める関数は以下のようになる", \t -> do
	itext t 1 "\\w -> (\\h -> w / (h * h))", \t -> do
	text t "* この関数に70を与えてみよう", \t -> do
	itext t 1 "(\\w -> (\\h -> w / (h * h))) 70", \t -> do
	itext t 1 "\\h -> 70 / (h * h)"
	]

function4 :: Page
function4 = [ \t -> do
	writeTopTitle t "多引数関数"
	text t "", \t -> do
	text t "* BMIを求める関数に引数70を与えると以下の関数が返る", \t -> do
	itext t 1 "\\h -> 70 / (h * h)", \t -> do
	text t "* さらに1.70を与えてみよう", \t -> do
	itext t 1 "(\\h -> 70 / (h * h)) 1.70", \t -> do
	itext t 1 "70 / (1.70 * 1.70)", \t -> do
	itext t 1 "70 / 2.89", \t -> do
	itext t 1 "24.2..."
	]

function5 :: Page
function5 = [ \t -> do
	writeTopTitle t "多引数関数"
	text t "", \t -> do
	text t "* 多引数関数はもっと簡単に書ける", \t -> do
	itext t 1 "\\[引数1] -> (\\[引数2] -> [式])"
	arrowIText t 1 "\\[引数1] [引数2] -> [式]", \t -> do
	text t "* BMI関数は以下のように書ける", \t -> do
	itext t 1 "\\w -> (\\h -> w / (h * h))"
	arrowIText t 1 "\\w h -> w / (h * h)", \t -> do
	text t "* これにbmiという名前をつけよう", \t -> do
	itext t 1 "bmi = \\w h -> w / (h * h)", \t -> do
	text t "* 関数定義の構文糖を使うと", \t -> do
	itext t 1 "bmi w h = w / (h * h)"
	]

function6 :: Page
function6 = [ \t -> do
	writeTopTitle t "多引数関数"
	text t "", \t -> do
	text t "* functions.hsに以下を追加しよう", \t -> do
	itext t 1 "bmi w h = w / (h * h)", \t -> do
	text t "* 対話環境で試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> bmi 70 1.70", \t -> do
	itext t 1 "24.221453287197235"
	]

higher1 :: Page
higher1 = [ \t -> do
	writeTopTitle t "高階関数"
	text t "", \t -> do
	text t "* 関数も「値」なので関数の引数にも返り値にもなる", \t -> do
	text t "* 関数を返り値とする関数は「多引数関数」と同じだ", \t -> do
	text t "* 関数を引数とする関数の例として関数twiceを定義しよう", \t -> do
	text t "* functions.hsに追加しよう", \t -> do
	itext t 1 "twice f x = f (f x)", \t -> do
	text t "* 第1引数の関数を第2引数の値に対して2回適用する関数だ", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> twice double 3", \t -> do
	itext t 1 "12", \t -> do
	itext t 1 "*Main> twice not False", \t -> do
	itext t 1 "False"
	]

operator1 :: Page
operator1 = [ \t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* 演算子は2引数関数だ", \t -> do
	text t "* 対話環境で以下を試してみよう", \t -> do
	itext t 1 "*Main> 12 + 25", \t -> do
	itext t 1 "37", \t -> do
	itext t 1 "*Main> (+) 12 25", \t -> do
	itext t 1 "37", \t -> do
	text t "* 演算子は中置記法ができる特殊な関数だ", \t -> do
	text t "* ()でかこってやることで通常の前置記法が可能となる", \t -> do
	text t "* 逆に普通の関数を``でかこむと中置記法ができる", \t -> do
	itext t 1 "*Main> 70 `bmi` 1.70", \t -> do
	itext t 1 "24.221453287197235"
	]

operator2 :: Page
operator2 = [ \t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* 演算子も自分で定義できる", \t -> do
	itext t 1 "w </> h = w / (h * h)", \t -> do
	text t "* </>という名前の演算子を定義した", \t -> do
	text t "* 関数リテラルで書くと", \t -> do
	itext t 1 "(</>) = \\w h -> w / (h * h)"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 関数は値を別の値に関連づけるもの", \t -> do
	text t "* 関数の定義は以下のような形となる", \t -> do
	itext t 1 "fun x y z = x + y * z", \t -> do
	text t "* 関数定義は関数を作って変数を束縛している", \t -> do
	itext t 1 "fun = \\x y z -> x + y * z", \t -> do
	text t "* 多引数関数の正体は「関数を返す関数」", \t -> do
	text t "* 演算子は中置記法の関数", \t -> do
	text t "* 演算子と通常の関数は()や``で相互に変換できる", \t -> do
	itext t 1 "12 + 25"
	preLine t
	arrowIText t 3 "(+) 12 25", \t -> do
	itext t 1 "bmi 70 1.70"
	preLine t
	arrowIText t 3.5 "70 `bmi` 1.70"
	]

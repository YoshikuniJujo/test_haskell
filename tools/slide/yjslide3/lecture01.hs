import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第1回 関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, function, apply,
	defineFun, aboutNano, defineFun2, defineFun3, funSummary,
	partial, partial2, partial3, partial4, partialSummary,
	literal, literal2, literal3, literal4, literalSummary,
	higherOrder, higherOrder2, higherOrderSummary,
	operator, operator2, operator3,
	dot, dollar,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでプログラミングをするということは", \t -> do
	itext t 1 "関数を様々なやりかたで組み合わせるということ", \t -> do
	text t "* 関数の定義のしかたを学ぶ", \t -> do
	text t "* 関数への引数の与えかたを学ぶ", \t -> do
	text t "* 関数の部分適用について見る", \t -> do
	text t "* リテラルとして関数を書く記法を学ぶ", \t -> do
	text t "* 高階関数について学ぶ", \t -> do
	text t "* Haskellでは演算子と関数が同じものであることを見る", \t -> do
	text t "* 便利な演算子(.)と($)を紹介する"
 ]

function :: Page
function = [\t -> do
	writeTopTitle t "関数の定義"
	text t "", \t -> do
	text t "* C言語では以下のようにする", \t -> do
	itext t 1 "int add(int x, int y) { return x + y; }", \t -> do
	text t "* 同じことをHaskellでは以下のようにする", \t -> do
	itext t 1 "add x y = x + y", \t -> do
	text t "* [関数名] [仮引数1] [仮引数2] ... = [表現]という形", \t -> do
	text t "* '='を使っているのはなぜか?", \t -> do
	itext t 1 "add x yはx + yに置き換えられるということ", \t -> do
	itext t 1 "プログラム中のadd 3 8は3 + 8に置き換え可能"
 ]

apply :: Page
apply = [\t -> do
	writeTopTitle t "関数の適用"
	text t "", \t -> do
	text t "* addの仮引数x, yに実引数3, 8を入れるには以下のように", \t -> do
	itext t 1 "add 3 8", \t -> do
	text t "* [関数名] [実引数1] [実引数2] ...という形", \t -> do
	text t "* [関数名]([実引数1], [実引数2], ...)のようにしない", \t -> do
	text t "* 空白でくぎった引数をならべるだけ", \t -> do
	text t "* この記法の合理性についてはそのうち明らかになる"
 ]

defineFun :: Page
defineFun = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* ghciの対話環境内でも関数を定義できる", \t -> do
	text t "* しかし、関数が長くなるとわずらわしいので", \t -> do
	text t "* 別ファイルで関数を定義し対話環境で使うことにする", \t -> do
	text t "* お好きなエディタを選んで、と言いたいところだが", \t -> do
	itext t 1 "nano、vim、Emacsしか用意していない", \t -> do
	text t "* vimやEmacsを使う人は「いつも通りに」編集してほしい", \t -> do
	text t "* 演者はvim使いなのでvimについては教えられるが", \t -> do
	itext t 1 "- Emacsについてはわからない", \t -> do
	text t "* これらのどのエディタも使ったことがないという人は", \t -> do
	itext t 1 "- とりあえずnanoを使っておこう"
 ]

aboutNano :: Page
aboutNano = [\t -> do
	writeTopTitle t "nano"
	text t "", \t -> do
	text t "* vimやEmacsを使う人は以下は気にしないで良い", \t -> do
	text t "* nanoは非常にシンプルなエディタである", \t -> do
	text t "* foo.txtを編集して練習してみよう", \t -> do
	itext t 1 "% cd ~/lectures/lecture01", \t -> do
	itext t 1 "% nano -w foo.txt", \t -> do
	text t "* 適当に入力しよう。例えば", \t -> do
	itext t 1 "foo bar baz", \t -> do
	text t "* 保存するにはCtrl-Oとする", \t -> do
	itext t 1 "- 保存するファイル名の確認のプロンプトが出るので", \t -> do
	itext t 1 "- そのままリターンを入力する", \t -> do
	text t "* 終了するにはCtrl-Xとすれば良い"
 ]

defineFun2 :: Page
defineFun2 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* 例題: 身長と体重を入力するとBMIを返す関数bmiを作る", \t -> do
	itext t 1 "BMI = [体重(kg)] / [身長(m)]の2乗", \t -> do
	text t "* bmi.hsファイルを作ろう", \t -> do
	itext t 1 "% cd ~/lectures/lecture01", \t -> do
	itext t 1 "% [エディタ] bmi.hs", \t -> do
	itext t 2 "- [エディタ]はnano -w, vim, emacsのどれか", \t -> do
	itext t 2 "- nanoを使う場合は-wオプションを忘れないように", \t -> do
	text t "* 今後は[エディタ]と書く代わりにnano -wとする", \t -> do
	text t "* それぞれviやemacsに読み換えてほしい"
 ]

bmi :: Double -> Double -> Double
bmi h w = w / (h / 100) ^ (2 :: Int)

defineFun3 :: Page
defineFun3 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* 以下の内容を書き込む", \t -> do
	itext t 1 "bmi h w = w / (h / 100) ^ 2", \t -> do
	text t "* 別のターミナルでghciにこのファイルを読み込ませる", \t -> do
	itext t 1 "% cd ~/lectures/lecture01/", \t -> do
	itext t 1 "% ghci bmi.hs", \t -> do
	itext t 1 "*Main> ", \t -> do
	itext t 2 "- \"Prelude>\"ではなく\"*Main>\"になった", \t -> do
	itext t 2 "- 理由については後々明らかになる", \t -> do
	text t "* とりあえずチェ・ホンマンのBMIを求めてみよう", \t -> do
	itext t 1 "*Main> bmi 218 164", \t -> do
	itext t 1 $ show $ bmi 218 164
 ]

funSummary :: Page
funSummary = [\t -> do
	writeTopTitle t "ここまでのまとめ"
	text t "", \t -> do
	text t "* 関数とは入力を取り出力を返すもの", \t -> do
	text t "* Haskellでは以下のように定義する", \t -> do
	itext t 1 "[関数名] [仮引数1] [仮引数2] ... = [表現]", \t -> do
	text t "* 関数fに値xを与えることをxにfを適用するという", \t -> do
	text t "* 関数適用は以下のようにする", \t -> do
	itext t 1 "[関数名] [実引数1] [実引数2] ..."
 ]

partial :: Page
partial = [\t -> do
	writeTopTitle t "関数の部分適用"
	text t "", \t -> do
	text t "* 例えばチェホンマンのbmiの変化を見たいと思ったとする", \t -> do
	itext t 1 "% ghci bmi.hs", \t -> do
	itext t 1 "*Main> bmi 218 164", \t -> do
	itext t 1 $ show $ bmi 218 164, \t -> do
	itext t 2 "- 2012年4月ごろには24kg減量したらしい", \t -> do
	itext t 1 "*Main> bmi 218 140", \t -> do
	itext t 1 $ show $ bmi 218 140, \t -> do
	text t "* 今後もチェホンマンのbmiを追っていきたいとする", \t -> do
	text t "* チェホンマン専用の関数を作ろう"
 ]

partial2 :: Page
partial2 = [\t -> do
	writeTopTitle t "関数の部分適用"
	text t "", \t -> do
	text t "* bmi.hsに以下を追加", \t -> do
	itext t 1 "bmiCHM w = bmi 218 w", \t -> do
	text t "* もとのプロンプトにもどり以下を入力", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 2 "- これでファイルの再読み込みができる", \t -> do
	itext t 2 "- エディタと対話環境を両方開いておける"
 ]

bmiCHM :: Double -> Double
bmiCHM w = bmi 218 w

partial3 :: Page
partial3 = [\t -> do
	writeTopTitle t "関数の部分適用"
	text t "", \t -> do
	text t "* 使ってみる", \t -> do
	itext t 1 "*Main> bmiCHM 164", \t -> do
	itext t 1 $ show $ bmiCHM 164, \t -> do
	itext t 1 "*Main> bmiCHM 140", \t -> do
	itext t 1 $ show $ bmiCHM 140, \t -> do
	text t "* bmiCHMの定義をもう一度見てみよう", \t -> do
	itext t 1 "bmiCHM w = bmi 218 w", \t -> do
	text t "* bmiCHMはbmiの第一引数のみを指定したものと言える", \t -> do
	text t "* 以下のように書くこともできる", \t -> do
	itext t 1 "bmiCHM = bmi 218"
 ]

partial4 :: Page
partial4 = [\t -> do
	writeTopTitle t "関数の部分適用"
	text t "", \t -> do
	text t "* bmi 218のような形を「部分適用」と呼ぶ", \t -> do
	text t "* 関数の一部の引数だけを指定した形", \t -> do
	text t "* bmi 218自体も関数なので", \t -> do
	itext t 1 "(bmi 218) 164のような形で引数を与えることが可能", \t -> do
	text t "* 今まで2つの引数を与えていたと思っていたが", \t -> do
	itext t 1 "本当はbmi 218 164は(bmi 218) 164の括弧を省略した形だった", \t -> do
	text t "* Haskellに2引数関数はない!", \t -> do
	text t "* bmiは引数を与えると「引数をひとつ取る関数」を返す関数", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "*Main> (bmi 218) 164", \t -> do
	itext t 1 $ show $ (bmi 218) 164
 ]

partialSummary :: Page
partialSummary = [\t -> do
	writeTopTitle t "関数の部分適用(まとめ)"
	text t "", \t -> do
	text t "* Haskellの2引数関数のように見えるものは", \t -> do
	itext t 1 "引数を取って「ひとつ引数を取る関数」を返す関数", \t -> do
	text t "* 関数適用は左結合", \t -> do
	itext t 1 "bmi 218 164は(bmi 218) 164と解釈される", \t -> do
	text t "* 次回扱う「型」について学ぶとより明らかになる"
 ]

fun :: Int -> Int
fun x = x ^ x

l1Number1 :: Int
l1Number1 = unsafePerformIO $ randomRIO (3, 8)

literal :: Page
literal = [\t -> do
	writeTopTitle t "関数リテラル"
	text t "", \t -> do
	text t "* たとえば数を利用するときにはいちいち名前をつけない", \t -> do
	text t "* 2とか3とかそのまま書けばいい", \t -> do
	text t "* これをリテラルと呼ぶ", \t -> do
	text t "* 今までのやりかただと名前のない関数は作れない", \t -> do
	text t "* 「引数xに対してxのx乗を返す関数」を作りたいとする", \t -> do
	text t "* その関数は一度しか使わないとする", \t -> do
	text t "* 今までのやりかただと以下のようになる", \t -> do
	itext t 1 "fun x = x ^ x", \t -> do
	itext t 1 $ "*Main> fun " ++ show l1Number1, \t -> do
	itext t 1 $ show $ fun l1Number1
 ]

literal2 :: Page
literal2 = [\t -> do
	writeTopTitle t "関数リテラル"
	text t "", \t -> do
	text t "* 一回しか使わないのに名前をつける必要はない", \t -> do
	text t "* 関数リテラルの記法を使うと無名関数を作ることができる", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 $ "*Main> (\\x -> x ^ x) " ++ show l1Number1, \t -> do
	itext t 1 $ show $ (\x -> x ^ x) l1Number1, \t -> do
	text t "* \\[仮引数] -> [表現]という形"
 ]

literal3 :: Page
literal3 = [\t -> do
	writeTopTitle t "関数リテラル"
	text t "", \t -> do
	text t "* 2引数関数が「関数を返す関数」であることを"
	itext t 1 "より直接的に表現すると以下のようになる", \t -> do
	itext t 1 "bmi h = \\w -> w / (h / 100) ^ 2", \t -> do
	itext t 1 "- 「wをとってw / (h / 100) ^ 2を返す関数」を返す", \t -> do
	text t "* さらに同様に以下のように書き換えることが可能", \t -> do
	itext t 1 "bmi = \\h -> \\w -> w / (h / 100) ^ 2", \t -> do
	text t "* 右辺のリテラル表記で作られた関数をbmiに束縛している", \t -> do
	text t "* リテラルを使わない関数定義は2つのことを同時にしていた", \t -> do
	itext t 1 "1. 関数の作成", \t -> do
	itext t 1 "2. 作成した関数の変数への束縛"
 ]

literal4 :: Page
literal4 = [\t -> do
	writeTopTitle t "関数リテラル"
	text t "", \t -> do
	text t "* 複数の引数をとる関数リテラルは省略記法がある", \t -> do
	itext t 1 "\\h -> \\w -> w / (h / 100) ^ 2", \t -> do
	arrowIText t 1 "\\h w -> w / (h / 100) ^ 2", \t -> do
	text t "* \\[引数1] -> \\[引数2] -> ... -> [表現]を省略して", \t -> do
	itext t 1 "\\[引数1] [引数2] ... -> [表現]"
 ]

literalSummary :: Page
literalSummary = [\t -> do
	writeTopTitle t "関数リテラル(まとめ)"
	text t "", \t -> do
	text t "* 関数リテラルの記法を使うと無名関数を書くことができる", \t -> do
	text t "* 記法は以下の通り", \t -> do
	itext t 1 "\\[引数1] [引数2] ... -> [表現]", \t -> do
	text t "* 普通の関数定義は構文糖と考えることができる", \t -> do
	itext t 1 "[関数名] [引数1] [引数2] ... = [表現]", \t -> do
	arrowIText t 1 "[関数名] = \\[引数1] [引数2] ... -> [表現]"
 ]

higherOrder :: Page
higherOrder = [\t -> do
	writeTopTitle t "高階関数"
	text t "", \t -> do
	text t "* 以下のどちらかを満たす関数を高階関数と呼ぶ", \t -> do
	itext t 1 "- 引数として関数をとる", \t -> do
	itext t 1 "- 返り値として関数を返す", \t -> do
	text t "* 高階関数を使うと", \t -> do
	itext t 1 "- くりかえし等の「構造」を関数で表現できる", \t -> do
	itext t 1 "- 他の言語での「構文」を普通の関数として定義できる", \t -> do
	text t "* 返り値として関数を返す関数は", \t -> do
	itext t 1 "- 高階関数として考える場合もあるが", \t -> do
	itext t 1 "- 複数の引数をとる関数として見る場合もある"
	{-
	text t "* Haskellでの2引数関数は本当は関数を返す関数なので", \t -> do
	itext t 1 "- 高階関数と呼ぶことができる", \t -> do
	itext t 1 "- 同じ関数でも2引数関数としてとらえる"
	-}
--	text t "* 引数として関数をとる関数について見ていこう"
 ]

twice :: (a -> a) -> a -> a
twice f = f . f

ho2number1 :: Int
ho2number1 = unsafePerformIO $ randomRIO (2, 9)

higherOrder2 :: Page
higherOrder2 = [\t -> do
	writeTopTitle t "高階関数"
	text t "", \t -> do
	text t "* 与えられた関数を与えられた値に2回適用する関数", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "% nano -w higher.hs", \t -> do
	itext t 1 "twice f x = f (f x)", \t -> do
	itext t 1 "*Main> :load higher.hs", \t -> do
	itext t 1 $ "*Main> twice (\\x -> x * (x + 1)) " ++ show ho2number1, \t -> do
	itext t 1 $ show $ twice (\x -> x * (x + 1)) ho2number1, \t -> do
	text t "* twiceは第一引数として関数を取るので高階関数である"
 ]

higherOrderSummary :: Page
higherOrderSummary = [\t -> do
	writeTopTitle t "高階関数(まとめ)"
	text t "", \t -> do
	text t "* 引数または返り値が関数である関数を高階関数と呼ぶ", \t -> do
	text t "* 返り値が関数である関数は", \t -> do
	itext t 1 "Haskellでは複数の引数をとる関数と同じこと", \t -> do
	text t "* 引数が関数である関数を使うと", \t -> do
	itext t 1 "プログラムの「構造」を関数で表現することができる"
 ]

operator :: Page
operator = [\t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* Haskellの演算子は本質的には関数と同じもの", \t -> do
	text t "* 3 + 8は(+)という関数に引数として3と8を与えたということ", \t -> do
	text t "* 演算子は()でかこってやることで普通の関数として扱える", \t -> do
	itext t 1 "*Main> 3 + 8", \t -> do
	itext t 1 $ show (3 + 8 :: Int), \t -> do
	itext t 1 "*Main> (+) 3 8", \t -> do
	itext t 1 $ show ((+) 3 8 :: Int), \t -> do
	text t "* 逆に関数は`(バッククォート)でかこむと演算子となる", \t -> do
	itext t 1 "*Main> :load bmi.hs", \t -> do
	itext t 1 "*Main> 218 `bmi` 164", \t -> do
	itext t 1 $ show $ 218 `bmi` 164
 ]

operator2 :: Page
operator2 = [\t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* 演算子についても部分適用が可能", \t -> do
	text t "* 第1引数と第2引数の両者について部分適用ができる", \t -> do
	itext t 1 "*Main> (3 +) 8", \t -> do
	itext t 1 $ show ((3 +) 8 :: Int), \t -> do
	itext t 1 "*Main> (+ 8) 3", \t -> do
	itext t 1 $ show ((+ 8) 3 :: Int), \t -> do
	text t "* 関数由来の演算子についても同じことができる", \t -> do
	itext t 1 "*Main> (218 `bmi`) 164", \t -> do
	itext t 1 $ show $ (218 `bmi`) 164, \t -> do
	itext t 1 "*Main> (`bmi` 164) 218", \t -> do
	itext t 1 $ show $ (`bmi` 164) 218
 ]

operator3 :: Page
operator3 = [\t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* 演算子は関数と同様に自分で定義できる", \t -> do
	text t "* 演算子に使える記号はアスキーの範囲では以下のものがある", \t -> do
	itext t 1 "!#$%&*+./<=>?@\\^|-~", \t -> do
	text t "* 以下のように定義する", \t -> do
	itext t 1 "x @ y = x ^ 2 + y ^ 2", \t -> do
	text t "* 関数と同じような形での定義も可能", \t -> do
	itext t 1 "(@) x y = x ^ 2 + y ^ 2", \t -> do
	text t "* 逆に関数を演算子と同じ形で定義することも", \t -> do
	itext t 1 "h `bmi` w = w / (h / 100) ^ 2"
 ]

dot :: Page
dot = [\t -> do
	writeTopTitle t "ドット演算子"
	text t "", \t -> do
	text t "* 以下のように定義されるドット演算子が用意されている", \t -> do
	itext t 1 "f . g = \\x -> f (g x)", \t -> do
	text t "* (f . g) xで、まずはxにgを適用しその結果をfに適用する", \t -> do
	text t "* ベルトコンベアのように関数を次々と適用していくイメージ", \t -> do
	text t "* ドット演算子で関数を組み合わせることを関数合成と呼ぶ", \t -> do
	text t "* 以下のような書き換えが可能", \t -> do
	itext t 1 "fun x = x * 3 + 4", \t -> do
	arrowIText t 1 "fun = (+ 4) . (* 3)", \t -> do
	text t "* 「3倍して4を足す」を直接的に表現できる"
 ]

dot2 :: Page
dot2  = [\t -> do
	writeTopTitle t "ドット演算子"
	text t "", \t -> do
	text t "* fun x = f (g x)という形はしばしば使われる", \t -> do
	text t "* xにgを適用してその結果にfを適用するということ", \t -> do
	text t "* もっと長くなってこんなふうになることも", \t -> do
	itext t 1 "fun2 x = f1 (f2 (f3 (f4 (f5 (f6 (f7 x))))))", \t -> do
	text t "* ドット演算子を使えばそれぞれ以下のように書ける", \t -> do
	itext t 1 "fun = f . g", \t -> do
	itext t 2 "- funは関数fと関数gを合成したもの", \t -> do
	itext t 1 "fun2 = f1 . f2 . f3 . f4 . f5 . f6 . f7", \t -> do
	itext t 2 "- fun2は関数f1から関数f7を順に合成したもの"
 ]

dollar :: Page
dollar = [\t -> do
	writeTopTitle t "$演算子"
	text t "", \t -> do
	text t "* 以下のような演算子も用意されている", \t -> do
	itext t 1 "f $ x = f x", \t -> do
	text t "* f $ xで値xに関数fを適用する", \t -> do
	text t "* つまり、f $ xはf xと同じこと", \t -> do
	text t "* 意味のない演算子のように見えるが", \t -> do
	itext t 1 "- 優先順位が低く右結合するように作られているので", \t -> do
	itext t 1 "- ()を省略したいときに使える", \t -> do
	text t "* 以下のような書き換えが可能になる", \t -> do
	itext t 1 "f (g (h (i (j (k x)))))", \t -> do
	arrowIText t 1 "f $ g $ h $ i $ j $ k x"
 ]

aboutIterate3 :: Page
aboutIterate3 = [\t -> do
	writeTopTitle t "高階関数"
	text t "", \t -> do
	text t "* 「リストの値のすべてを二乗したリストを作る」を考える", \t -> do
	text t "* 「リストの値のすべてに何かする」という構造が抽出できる", \t -> do
	text t "* 多くの言語でこのような「構造」は「構文」として特別扱い", \t -> do
	itext t 1 "- C言語のfor文等", \t -> do
	text t "* 引数として関数を取る関数を作れる言語では", \t -> do
	itext t 1 "「構造」を関数として定義することが可能", \t -> do
	text t "* 引数として関数を取る関数は高階関数と呼ばれる", \t -> do
	text t "* 「リストの値のすべてに何かする」関数は用意されている", \t -> do
	itext t 1 "- map f lstでlstのすべての要素にfを適用する"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 関数定義: [関数名] [仮引数1] [仮引数2] ... = [表現]", \t -> do
	text t "* 関数適用: [関数名] [実引数1] [実引数2] ...", \t -> do
	text t "* 複数の引数をとる関数は「関数を返す関数」である", \t -> do
	text t "* 関数リテラル: \\[仮引数1] [仮引数2] ... -> [表現]", \t -> do
	text t "* 引数か返り値が関数であるような関数を高階関数と呼ぶ", \t -> do
	text t "* 演算子と関数は()と``で相互に変換可能", \t -> do
	text t "* 演算子も関数も部分適用が可能", \t -> do
	text t "* 便利な演算子(.)と($)が用意されている"
 ]

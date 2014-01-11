import System.Random
import System.IO.Unsafe

import Lecture

subtitle :: String
subtitle = "第1回 関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, function, apply,
	defineFun, defineFun2, defineFun3, funSummary,
	partial, partial2, partial3, partial4, partialSummary,
	literal, literal2, literal3, literal4, literalSummary,
	higherOrder, higherOrder2, higherOrderSummary,
	dot,
	operator, operator2
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
	text t "* Haskellでは演算子と関数が同じものであることを見る"
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
	text t "* '='を使っているのは何故か?", \t -> do
	itext t 1 "add x yはx + yに置き換えられるということ", \t -> do
	itext t 1 "プログラム中のadd 3 8は3 + 8に置き換え可能"
 ]

apply :: Page
apply = [\t -> do
	writeTopTitle t "関数の適用"
	text t "", \t -> do
	text t "* 「関数の定義」でもすこし触れたが", \t -> do
	text t "* addの仮引数x, yに実引数3, 8を入れるには以下のように", \t -> do
	itext t 1 "add 3 8", \t -> do
	text t "* [関数名] [実引数1] [実引数2] ...という形", \t -> do
	text t "* 関数適用の際に関数名のあとに(..., ...)のようにしない", \t -> do
	text t "* 単に引数を並べれば良い", \t -> do
	text t "* この記法の合理性については後々明らかになる"
 ]

defineFun :: Page
defineFun = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* ghciの対話環境内でも関数を定義できる", \t -> do
	text t "* しかし、関数が長くなるとわずらわしいので", \t -> do
	text t "* 別ファイルで関数を定義し、", \t -> do
	itext t 1 "対話環境でそれを使うことにしよう", \t -> do
	text t "* お好きなエディタを選んで、と言いたいところだが", \t -> do
	itext t 1 "- メモ帳、Vim、Emacsしか用意していない", \t -> do
	itext t 2 "(著者メモ) Emacsは用意できるかどうか"
 ]

defineFun2 :: Page
defineFun2 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* 適当なフォルダを作る", \t -> do
	itext t 1 "% cd ~ (Linuxの場合)"
	itext t 1 "% cd %userprofile% (Windowsの場合)", \t -> do
	itext t 1 "% mkdir lectures/lecture01", \t -> do
	itext t 1 "% cd lectures/lecture01", \t -> do
	text t "* 例題: 身長と体重を入力するとBMIを返す関数bmiを作る", \t -> do
	itext t 1 "BMI = [体重(kg)] / [身長(m)]の2乗", \t -> do
	text t "* bmi.hsファイルを作ろう", \t -> do
	itext t 1 "% [エディタ] bmi.hs", \t -> do
	itext t 2 "[エディタ]はnotepad, vim, emacsのどれか"
 ]

bmi :: Double -> Double -> Double
bmi h w = w / (h / 100) ^ (2 :: Int)

defineFun3 :: Page
defineFun3 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* 以下の内容を書き込む", \t -> do
	itext t 1 "bmi h w = w / (h / 100) ^ 2", \t -> do
	text t "* ghciにこのファイルを読み込ませる", \t -> do
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
	text t "* コマンドプロンプトをもうひとつ立ち上げてみる", \t -> do
	itext t 1 "ウィンドウズキー + R -> cmd", \t -> do
	itext t 1 "% cd lecture/lecture01/", \t -> do
	itext t 1 "% [エディタ] bmi.hs", \t -> do
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
	text t "* 実は今まで2つの引数を与えていたと思っていたが", \t -> do
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
	text t "* そしてその関数は一度しか使わないとする", \t -> do
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
	itext t 1 "\\[引数1] [引数2] -> [表現]", \t -> do
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
	text t "* Haskellでの2引数関数は本当は関数を返す関数なので", \t -> do
	itext t 1 "高階関数と呼ぶことができる", \t -> do
	text t "* 高階関数を使うと", \t -> do
	itext t 1 "- くりかえしや分岐等の「構造」を関数で表現できる", \t -> do
	itext t 1 "- 他の言語での「構文」を普通の関数として定義できる"
--	text t "* 引数として関数をとる関数について見ていこう"
 ]

twice :: (a -> a) -> a -> a
twice f = f . f

higherOrder2 :: Page
higherOrder2 = [\t -> do
	writeTopTitle t "高階関数"
	text t "", \t -> do
	text t "* 与えられた関数を与えられた値に2回適用する関数", \t -> do
	itext t 1 "% [エディタ] higher.hs", \t -> do
	itext t 1 "twice f x = f (f x)", \t -> do
	itext t 1 "% ghci higher.hs", \t -> do
	itext t 1 "*Main> twice (\\x -> x * (x + 1)) 3", \t -> do
	itext t 1 $ show $ twice (\x -> x * (x + 1)) (3 :: Int), \t -> do
	text t "* twiceは第一引数として関数を取るので高階関数である"
 ]

higherOrderSummary :: Page
higherOrderSummary = [\t -> do
	writeTopTitle t "高階関数(まとめ)"
	text t "", \t -> do
	text t "* 引数または返り値が関数である関数を高階関数と呼ぶ", \t -> do
	text t "* 返り値が関数である関数は", \t -> do
	itext t 1 "Haskellでは普通の複数の引数をとる関数と同じこと", \t -> do
	text t "* 引数が関数である関数を使うと", \t -> do
	itext t 1 "プログラムの「構造」を関数で表現することができる"
 ]

dot :: Page
dot  = [\t -> do
	writeTopTitle t "ドット演算子"
	text t ""
 ]

operator :: Page
operator = [\t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* Haskellの演算子は本質的には関数と同じもの", \t -> do
	text t "* 3 + 8は+という関数に入力として3と8を与えたということ", \t -> do
	text t "* 演算子は()でかこってやることで普通の関数として扱える", \t -> do
	itext t 1 "*Main> 3 + 8", \t -> do
	itext t 1 $ show (3 + 8 :: Int), \t -> do
	itext t 1 "*Main> (+) 3 8", \t -> do
	itext t 1 $ show ((+) 3 8 :: Int), \t -> do
	text t "* 逆に関数は``(バッククォート)でかこむと演算子となる", \t -> do
	itext t 1 "*Main> 218 `bmi` 164", \t -> do
	itext t 1 $ show $ 218 `bmi` 164
 ]

operator2 :: Page
operator2 = [\t -> do
	writeTopTitle t "演算子"
	text t "", \t -> do
	text t "* 演算子についても部分適用が可能", \t -> do
	text t "* 第一引数と第二引数の両者について部分適用ができる", \t -> do
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

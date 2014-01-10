import Lecture

subtitle :: String
subtitle = "第1回 関数・型・リスト"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, function, apply,
	defineFun, defineFun2, defineFun3
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellでプログラミングをするということは", \t -> do
	itext t 1 "関数を様々なやりかたで組み合わせるということ", \t -> do
	text t "* 型は関数を組み合わせる際に重要な役割を持つ", \t -> do
	text t "* プログラミングの機能として「くりかえし」は重要", \t -> do
	text t "* 「くりかえし」を実現するために", \t -> do
	itext t 1 "- 手続き型言語では状態変化を使う", \t -> do
	itext t 1 "- Haskellでは主にリストを使う", \t -> do
	text t "* 今回は関数・型・リストについて学ぼう"
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
	itext t 1 "- メモ帳、Vim、Emacsしか用意していない"
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

bmi h w = w / (h / 100) ^ 2

defineFun3 :: Page
defineFun3 = [\t -> do
	writeTopTitle t "やってみよう"
	text t "", \t -> do
	text t "* 以下の内容を書き込む", \t -> do
	itext t 1 "bmi h w = w / (h / 100) ^ 2", \t -> do
	text t "* ghciにこのファイルを読み込ませる", \t -> do
	itext t 1 "% ghci bmi.hs", \t -> do
	itext t 1 "Main> ", \t -> do
	itext t 2 "- \"Prelude>\"ではなく\"Main>\"になった", \t -> do
	itext t 2 "- 理由については後々明らかになる", \t -> do
	text t "* とりあえずチェ・ホンマンのBMIを求めてみよう", \t -> do
	itext t 1 "Main> bmi 218 160", \t -> do
	itext t 1 $ show $ bmi 218 160, \t -> do
	text t "* 標準が22なので体重オーバー", \t -> do
	preLine t
	itext t 5 "まあ格闘家なので"
 ]

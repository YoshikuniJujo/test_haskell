import Lecture

subtitle :: String
subtitle = "第11回 自作型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	enumerate, enumerate2, enumerate3,
	intAsEnumerate
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 今までは用意された型に対する演算を扱ってきた", \t -> do
	text t "* Haskellでは自分で型を作ることができる", \t -> do
	text t "* 型の作りかたには以下の方法が考えられる", \t -> do
	itext t 1 "- 値を列挙する", \t -> do
	itext t 1 "- 既存の型を組み合わせる", \t -> do
	text t "* Haskellでは両者が無理なく1つの枠組みにまとまっている", \t -> do
	text t "* すくなくとも意味論的には整数や文字型は", \t -> do
	itext t 1 "値を列挙することによって作られた型と考えられる"
 ]

enumerate :: Page
enumerate = [\t -> do
	writeTopTitle t "列挙する"
	text t "", \t -> do
	text t "* 値を列挙することで型を作ることができる", \t -> do
	text t "* 「値」は大文字ではじまる識別子", \t -> do
	itext t 1 "data Friend = Takashi | Kazuya | Keiko", \t -> do
	itext t 1 "data Sex = Man | Woman", \t -> do
	text t "* 3人の友達を表現する型Friendと", \t -> do
	text t "* 性別を表現する型Sexとを作った", \t -> do
	text t "* これらの型を扱う関数sexを作ってみる", \t -> do
	itext t 1 "sex :: Friend -> Sex", \t -> do
	itext t 1 "sex Takashi = Man", \t -> do
	itext t 1 "sex Kazuya = Man", \t -> do
	itext t 1 "sex Keiko = Woman"
 ]

enumerate2 :: Page
enumerate2 = [\t -> do
	writeTopTitle t "列挙する"
	text t "", \t -> do
	text t "* ここまでの定義をlectures/lecture11/data.hsに書きこもう", \t -> do
	itext t 1 "data Friend = Takashi | Kazuya | Keiko"
	itext t 1 "data Sex = Man | Woman"
	itext t 1 ""
	itext t 1 "sex :: Friend -> Sex"
	itext t 1 "sex Takashi = Man"
	itext t 1 "sex Kazuya = Man"
	itext t 1 "sex Keiko = Woman"
 ]

data Friend = Takashi | Kazuya | Keiko
data Sex = Man | Woman

sex :: Friend -> Sex
sex Takashi = Man
sex Kazuya = Man
sex Keiko = Woman

sexCheck :: Friend -> String
sexCheck f = case sex f of
	Man -> "He is a man."
	Woman -> "She is a woman."

enumerate3 :: Page
enumerate3 = [\t -> do
	writeTopTitle t "列挙する"
	text t "", \t -> do
	text t "* 男女を判定しメッセージを作成する", \t -> do
	itext t 1 "sexCheck :: Friend -> String"
	itext t 1 "sexCheck f = case sex f of"
	itext t 2 "Man -> \"He is a man.\""
	itext t 2 "Woman -> \"She is a woman.\"", \t -> do
	text t "* 上記の定義もdata.hsに書き込み", \t -> do
	itext t 1 "% ghci data.hs", \t -> do
	itext t 1 "*Main> sexCheck Takashi", \t -> do
	itext t 1 $ show $ sexCheck Takashi, \t -> do
	itext t 1 "*Main> sexCheck Keiko", \t -> do
	itext t 1 $ show $ sexCheck Keiko
 ]

intAsEnumerate :: Page
intAsEnumerate = [\t -> do
	writeTopTitle t "整数型"
	text t "", \t -> do
	text t "* 今まで使ってきたIntも数の列挙と考えられる", \t -> do
	itext t 1 "data Int ="
	itext t 2 "-2147483648 | -2147483647 | ... | -2 | -1 |"
	itext t 2 "0 | 1 | 2 | ... | 2147483646 | 2147483647"
 ]

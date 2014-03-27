import Lecture

subtitle :: String
subtitle = "自習課題(1日目)"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, problem, howTo,
	functions, howToZorome, allC, question
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 1日目の講義はこれで終了です", \t -> do
	itext t 1 "- お疲れさまでした", \t -> do
	text t "* 理解度を見るために自習課題を作成しました", \t -> do
	text t "* 可能であれば次週に提出していただけると幸いです"
 ]

problem :: Page
problem = [\t -> do
	writeTopTitle t "問題定義"
	text t "", \t -> do
	text t "* 小さいほうからn番目のぞろ目の和を求める"
 ]

howTo :: Page
howTo = [\t -> do
	writeTopTitle t "全体の流れ"
	text t "", \t -> do
	text t "* 10以上のすべての整数のリストは以下のように作れる", \t -> do
	itext t 1 "[10 ..]", \t -> do
	text t "* このリストから「ぞろ目」のみを取り出し", \t -> do
	text t "* さらにそのリストから先頭のn個を取り出し", \t -> do
	text t "* その総和を求める"
 ]

functions :: Page
functions = [\t -> do
	writeTopTitle t "新たに必要な関数"
	text t "", \t -> do
	text t "* show: 整数やその他の「値」を文字列に変換する", \t -> do
	text t "* all: 引数として(p :: a -> Bool)と(xs :: [a])を取り", \t -> do
	itext t 1 "xsのすべての値がpを満たすかどうか判定", \t -> do
	text t "* any: 引数として(p :: a -> Bool)と(xs :: [a])を取り", \t -> do
	itext t 1 "xsのどれかの値がpを満たすかどうか判定", \t -> do
	text t "* sum: 数のリストを引数として取り、その総和を求める"
 ]

howToZorome :: Page
howToZorome = [\t -> do
	writeTopTitle t "ぞろ目判定の流れ"
	text t "", \t -> do
	text t "* 数字を文字列に変換する", \t -> do
	text t "* その文字列をリストとして見て以下を判定する", \t -> do
	itext t 1 "- 「すべての要素が'1'である」または", \t -> do
	itext t 1 "- 「すべての要素が'2'である」または", \t -> do
	itext t 1 "- ...または", \t -> do
	itext t 1 "- 「すべての要素が'9'である」"

 ]

allC :: Page
allC = [\t -> do
	writeTopTitle t "all1To9"
	text t "", \t -> do
	text t "* 「すべての要素が'1'である」を判定する関数", \t -> do
	itext t 1 "all1 :: String -> Bool", \t -> do
	itext t 1 "all1 = all (== '1')", \t -> do
	text t "* 「すべての要素がcである」を判定する関数", \t -> do
	itext t 1 "allC :: Char -> String -> Bool", \t -> do
	itext t 1 "allC c = all (== c)", \t -> do
	text t "* 「すべての要素が'1'である」または ...", \t -> do
	itext t 1 "「すべての要素が'9'である」を判定する関数", \t -> do
	itext t 0.5 "all1To9 :: String -> Bool", \t -> do
	itext t 0.5 "all1To9 str = any (\\c -> all (== c) str) \"123456789\"", \t -> do
	text t "* \"123456789\"は文字'1'から'9'までのリストということ"
 ]

question :: Page
question = [\t -> do
	writeTopTitle t "問い"
	text t "", \t -> do
	text t "* 「小さいほうからn個のぞろ目の総和を求める関数」を", \t -> do
	itext t 1 "以下の関数を使って作成せよ", \t -> do
	itext t 1 "filter, show, all1To9, take, sum", \t -> do
	itext t 1 "10以上の整数のリストは[10 ..]で入手できる", \t -> do
	text t "* Windows PCでのghcをインストールするには", \t -> do
	itext t 1 "以下のexeを実行すれば良い"
	text t "", \t -> do
	text t "http://www.haskell.org/platform/download/2013.2.0.0/"
	itext t 1 "HaskellPlatform-2013.2.0.0-setup.exe"
 ]

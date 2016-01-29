import Lecture

subtitle :: String
subtitle = "5. 再帰関数"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, listSum, rec, summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 関数型言語では再帰的な定義がよく使われる", \t -> do
	text t "* 多くの処理は再帰的に定義できる", \t -> do
	text t "* 再帰的な定義は表現力が高い"
	]

listSum :: Page
listSum = [ \t -> do
	writeTopTitle t "リストの総和"
	text t "", \t -> do
	text t "* リストは以下のような構造の値である", \t -> do
	itext t 1 "5 : 7 : 8 : 2 : 9 : 1 : []", \t -> do
	text t "* わかりやすいように部分的に構文糖を使うと", \t -> do
	itext t 1 "5 : [7, 8, 2, 9, 1]", \t -> do
	text t "* リストとはリストの先頭に要素を追加したものだ", \t -> do
	text t "* 空リストの総和は0であり", \t -> do
	text t "* リストxsの先頭に値xを追加したものの総和は", \t -> do
	itext t 1 "リストxsの総和に値xを足したものとなる", \t -> do
	text t "* そのまま書けばよい", \t -> do
	itext t 1 "sum [] = 0", \t -> do
	itext t 1 "sum (x : xs) = x + sum xs"
	]

rec :: Page
rec = [ \t -> do
	writeTopTitle t "再帰的定義"
	text t "", \t -> do
	text t "* リストの総和を求める関数", \t -> do
	itext t 1 "sum [] = 0", \t -> do
	itext t 1 "sum (x : xs) = x + sum xs", \t -> do
	text t "* 関数sumの定義に関数sumが使われている", \t -> do
	text t "* このような定義のしかたを「再帰的定義」と呼ぶ"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 再帰的定義の例を見た"
	]

import Lecture

subtitle :: String
subtitle = "第39回 FlexibleContexts拡張"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* FlexibleContextsという言語拡張がある", \t -> do
	text t "* 標準ではContextはClass varという形", \t -> do
	text t "* 複数引数クラスに対してはClass var1 var2 ...という形", \t -> do
	text t "* いずれにしてもクラスに与えられる引数は単純な型変数", \t -> do
	text t "* FlexibleInstances拡張なしで作られたクラスには十分", \t -> do
	itext t 1 "- インスタンス定義にCons v1 v2 ...という形", \t -> do
	itext t 1 "- Cons ...がクラスに属するかどうかは", \t -> do
	itext t 2 "v1, v2 ...に対する文脈のみで定まる"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* FlexibleInstances拡張のもとで作られたクラスには", \t -> do
	itext t 1 "- Cons v1 v2 ... C1 ... C2という形がある", \t -> do
	itext t 1 "- Cons v1 v2 ... vk ... vkという形もある", \t -> do
	arrowIText t 1 "Cons ...がクラスに属するかどうかは", \t -> do
	itext t 2 "v1, v2 ...に対する文脈のみでは定まらない", \t -> do
	text t "* Class (Cons ...)という形の文脈が必要になってくる"
 ]

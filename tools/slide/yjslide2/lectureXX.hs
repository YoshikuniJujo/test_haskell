import Lecture

subtitle :: String
subtitle = "第XX回 C言語とHaskell"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	[flip writeTitle subtitle], prelude, semicolon, functional
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* C言語とHaskellを比較してみる"
 ]

semicolon :: Page
semicolon = [\t -> do
	writeTopTitle t "セミコロン"
	text t "", \t -> do
	text t "* C言語では順接を';'で表す", \t -> do
	text t "* Haskellでは'>>'を使いこれは演算子である"
 ]

functional :: Page
functional = [\t -> do
	writeTopTitle t "返り値と手続き"
	text t "", \t -> do
	text t "* アセンブリ言語は純粋な手続き型言語", \t -> do
	itext t 1 "- 返り値というものはない", \t -> do
	text t "* C言語には関数型言語的な面もある", \t -> do
	itext t 1 "- 返り値というものがある", \t -> do
	text t "* しかし、手続き型的な側面が強い", \t -> do
	itext t 1 "- 返り値ではなくポインタを渡すことで値を変化させる", \t -> do
	text t "* これと返り値とある"
 ]

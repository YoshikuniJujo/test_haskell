import Lecture

subtitle :: String
subtitle = "第24回 最後に"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Haskellの基礎を学んだ", \t -> do
	text t "* Haskellを学ぶということは「考えかた」を学ぶということ", \t -> do
	text t "* 「関数型言語」は数学的基礎づけがしっかりしている", \t -> do
	text t "* 瑣末な問題にわずらわされにくい", \t -> do
	itext t 1 "- 解こうとしている問題に集中できる", \t -> do
	text t "* 手続き型言語では以下のようなことが起こりがち", \t -> do
	itext t 1 "- こっちをいじると関係ないあっちが変化してしまう", \t -> do
	text t "* Haskellはジョイント部分の「ゆるみ」が非常にすくない", \t -> do
	text t "* こっちの意図通りのことをしてくれる", \t -> do
	arrowIText t 1 "非常に優秀な道具である"
 ]

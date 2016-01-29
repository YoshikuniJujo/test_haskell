import Lecture

subtitle :: String
subtitle = "2. 型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	whatsType, tag, dec, function, summary
	]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t ""
	]

whatsType :: Page
whatsType = [ \t -> do
	writeTopTitle t "引数となれる値"
	text t "", \t -> do
	text t "* 関数は引数をとり返り値をかえす", \t -> do
	text t "* たとえばBool値を反転させる関数notがある", \t -> do
	text t "* この関数にFalseをあたえるとTrueがかえる", \t -> do
	text t "* TrueをあたえるとFalseがかえる", \t -> do
	text t "* では文字'c'をあたえるとどうなるだろうか", \t -> do
	text t "* 関数notはBool値のみに対して定義されている", \t -> do
	text t "* よってBool値以外に適用することは意味を持たない", \t -> do
	arrowIText t 1 "無意味な関数適用は機械的にチェックしたい"
	]

tag :: Page
tag = [ \t -> do
	writeTopTitle t "タグ"
	text t "", \t -> do
	text t "* 値にタグをつける", \t -> do
	text t "* FalseやTrueにはBoolというタグをつける", \t -> do
	text t "* 'a', 'b', 'c'などにはCharというタグをつける", \t -> do
	text t "* このタグを「型」と呼ぶ", \t -> do
	text t "* 関数notの引数となれるのはBool型の値だけだ", \t -> do
	text t "* 機械的なチェックが可能となる", \t -> do
	text t "* しかしこれだけだとnot xの型が定まらない", \t -> do
	text t "* 関数notが返り値としてBool型の値をかえすことを明示する", \t -> do
	text t "* 返り値の型を明示することで型チェックを連鎖させられる"
	]

dec :: Page
dec = [ \t -> do
	writeTopTitle t "型宣言"
	text t "", \t -> do
	text t "* 型は値の使いかたを示す説明書のようなもの", \t -> do
	itext t 1 "Bool型の値は関数notの引数や返り値となる", \t -> do
	itext t 1 "Char型の値は関数toLowerの引数や返り値となる", \t -> do
	itext t 1 "Char型の値は関数isLowerの引数となる", \t -> do
	itext t 1 "Bool型の値は関数isLowerの返り値となる", \t -> do
	text t "* コード上で型を明示することは優れたドキュメントとなる", \t -> do
	text t "* ファイルtypes.hsに以下を書きこもう", \t -> do
	itext t 1 "happy :: Bool", \t -> do
	itext t 1 "happy = True", \t -> do
	itext t 1 "", \t -> do
	itext t 1 "initial :: Char", \t -> do
	itext t 1 "initial = 'Y'"
	]

function :: Page
function = [ \t -> do
	writeTopTitle t "関数の型"
	text t "", \t -> do
	text t "* 関数も値だ", \t -> do
	text t "* 引数や返り値になる", \t -> do
	text t "* 関数にはどのような型がつくだろうか", \t -> do
	text t "* 関数の使いかたは引数と返り値のみで決まる", \t -> do
	text t "* 関数の型は引数と返り値から構成される", \t -> do
	text t "* types.hsに以下を書きこもう", \t -> do
	itext t 1 "notLower :: Char -> Bool", \t -> do
	itext t 1 "notLower c = not (isLower c)", \t -> do
	text t "* 関数isLowerを使うために先頭に以下を追加する", \t -> do
	itext t 1 "import Data.Char (isLower)"
	]

summary :: Page
summary = [ \t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* 関数は特定の値に対してしか定義されない", \t -> do
	text t "* 定義されない値に対する適用は機械的にチェックしたい", \t -> do
	text t "* 値をグループわけしてそれぞれに「型」をつける", \t -> do
	text t "* 関数の引数と返り値の型を決めると連鎖的にチェックできる", \t -> do
	text t "* 関数自体にも型があり引数の型と返り値の型とで示される"
	]

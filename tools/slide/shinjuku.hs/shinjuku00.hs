import Lecture

subtitle :: String
subtitle = "0. Hello, world!への道"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, chanto, plan, follow
 ]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Hello, world!をちゃんと理解してから言おうという話", \t -> do
	text t "* ちゃんと理解するためにはIOモナドを避けては通れない", \t -> do
	text t "* 「モナドは難しい」「IOモナドは難しい」と言われる", \t -> do
	text t "* そんなことはない", \t -> do
	text t "* ただし", \t -> do
	itext t 1 "+ 誤解しがちなところがある", \t -> do
	itext t 1 "+ わかったような気になる", \t -> do
	text t "* そういうところがある", \t -> do
	text t "* ちょっと考えるとわかるのだけど", \t -> do
	itext t 1 "ちゃんと考えるとわからなくなる"
	]

chanto :: Page
chanto = [ \t -> do
	writeTopTitle t "ちゃんと"
	text t "", \t -> do
	text t "* IOモナドを理解するには", \t -> do
	text t "* 関数と型", \t -> do
	text t "* 関数と型", \t -> do
	text t "* 大事なことなので2回言いました", \t -> do
	text t "* この2つをきちんと理解すればIOモナド恐れるに足りず", \t -> do
	text t "* 関数と型に対する理解がきちんと身についていれば", \t -> do
	itext t 1 "IOモナドなどたやすい", \t -> do
	text t "* 逆にそれらをきちんと身につけなければ", \t -> do
	itext t 1 "本当の理解は困難だ", \t -> do
	text t "* 関数と型について「ちゃんと」身につけよう"
	]

plan :: Page
plan = [ \t -> do
	writeTopTitle t "IOモナドへの(近)道", \t -> do
	text t " 1. 関数", \t -> do
	text t " 2. 型", \t -> do
	text t " 3. 多相関数", \t -> do
	text t " 4. 代数的データ型", \t -> do
	text t " 5. 再帰関数 <- 今日(#1)はココマデ", \t -> do
	text t " 6. 型クラス", \t -> do
	text t " 7. モノイド", \t -> do
	text t " 8. ファンクター", \t -> do
	text t " 9. アプリカティブファンクター <- 次回(#2)はココマデ", \t -> do
	text t "10. 関数の変形", \t -> do
	text t "11. モナド", \t -> do
	text t "12. IOモナド <- #3でHello, world!"
	]

follow :: Page
follow = [ \t -> do
	writeTopTitle t "聞きのがしたら"
	text t "", \t -> do
	text t "* 出席できなかったりして聞きのがしたとき", \t -> do
	text t "* 人数が少なければ個別に対応", \t -> do
	text t "* ある程度人数がいれば補講とします", \t -> do
	text t "* ついてこれなければSlackまたはメールで個別対応します"
	]

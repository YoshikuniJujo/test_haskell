import Lecture

subtitle :: String
subtitle = "第1回 方向性"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	[flip writeTitleTT subtitle], prelude, goal, virtue, virtue2,
	state, state2, cando, cando2, icando, wall, others
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 何も用意しないで話をするのも楽しい、が", \t -> do
	text t "* あとから、「あれも話せば良かった」等がある", \t -> do
	text t "* このごく簡単な、スライドシステムも見せたい", \t -> do
	text t "* ということで、スライドを作ってみた、が", \t -> do
	text t "* とくに、このスライドにとらわれないで話したい"
 ]

goal :: Page
goal = [\t -> do
	writeTopTitle t "何をしたいか"
	text t "", \t -> do
	text t "* できるだけ多くの人にHaskellの魅力を伝えたい", \t -> do
	text t "* 個人的なことを言えば、", \t -> do
	itext t 1 "- Haskellのことをして生活したい", \t -> do
	itext t 1 "- Haskellのこと「だけ」して生活できたら最高", \t -> do
	itext t 1 "- むしろ、様々なこととHaskellを結びつけていきたい", \t -> do
	text t "* そのためには", \t -> do
	itext t 1 "- Haskellの良さを人々に知ってもらうのが一番"
 ]

virtue :: Page
virtue = [\t -> do
	writeTopTitle t "良さを知ってもらうには"
	text t "", \t -> do
	text t "* 本当の良さを知るためにはある程度以上学ぶ必要がある", \t -> do
	text t "* 良さを(あるいはダメさを)知るレベルまでには", \t -> do
	itext t 1 "- それなりの労力を費す必要がある", \t -> do
	text t "* その労力を費すためには「楽しむ」必要がある", \t -> do
	text t "* 何が「楽しい」のかは人それぞれだ、が", \t -> do
	text t "* プログラミングが好きな人にはある程度の共通点がある", \t -> do
	text t "* 楽しむためには、広い視野と深い視点との両方がほしい", \t -> do
	itext t 1 "- 広い視野: 他の言語との比較や関数型言語の位置づけ", \t -> do
	itext t 1 "- 深い視点: Haskellにどっぷりつかってみる"
 ]

virtue2 :: Page
virtue2 = [\t -> do
	writeTopTitle t "良さを知ってもらうには"
	text t "", \t -> do
	text t "* 楽しさ、だけではなく有用性もアピールしたい", \t -> do
	text t "* Haskell自体を使うことのメリットもある、が", \t -> do
	text t "* Haskellを学ぶことで「良いプログラム」についての", \t -> do
	itext t 1 "- 考えが深まる", \t -> do
	itext t 1 "- 感覚がみがかれる", \t -> do
	text t "* 「明日からのプログラミングが変わる」", \t -> do
	itext t 1 "というキャッチーなフレーズ", \t -> do
	text t "* 不必要なグローバル変数を使っている関数を見ると", \t -> do
	itext t 1 "背筋が寒くなる、ようになる"
 ]

state :: Page
state = [\t -> do
	writeTopTitle t "状態変化について"
	text t "", \t -> do
	text t "* 関数型言語の参照透過性を", \t -> do
	itext t 1 "オブジェクト指向の延長線上に位置づけられる", \t -> do
	text t "* オブジェクト指向とは", \t -> do
	itext t 1 "「関数が状態を持つ」->「オブジェクトが状態を持つ」", \t -> do
	text t "* 「関数が状態を持つ」場合", \t -> do
	itext t 1 "その関数が使われ始めてからを、ずっと追う必要がある", \t -> do
	text t "* 「オブジェクトが状態を持つ」ならば", \t -> do
	itext t 1 "オブジェクトの生成以降を追えばすむ", \t -> do
	arrowIText t 1 "状態変化の局所化"
 ]

state2 :: Page
state2 = [\t -> do
	writeTopTitle t "状態変化について"
	text t "", \t -> do
	text t "* 関数型言語では", \t -> do
	itext t 1 "状態変化自体がない", \t -> do
	arrowIText t 1 "定義の場所と使われている場所だけを見れば良い", \t -> do
	text t "* 状態変化がない", \t -> do
	itext t 1 "- 手続き型言語では「何をするか」を記述する", \t -> do
	itext t 1 "- 関数型言語では「何であるか」を記述する", \t -> do
	text t "* 「何であるか」を記述する言語を宣言型言語と呼ぶ", \t -> do
	text t "* 宣言型言語には関数型言語の他に論理型言語がある"
 ]

cando :: Page
cando = [\t -> do
	writeTopTitle t "Haskellで何ができるか"
	text t "", \t -> do
	text t "* 今のところできないこと", \t -> do
	itext t 1 "- androidアプリは作れない", \t -> do
	itext t 1 "- その他Javaの資産は使えない", \t -> do
	text t "* ウェブアプリは作れる(yesodがある)", \t -> do
	text t "* 出荷するタイプのソフトウェアなら何でも作れる", \t -> do
	itext t 1 "- コンパイラがあるので", \t -> do
	text t "* 木構造を扱うプログラミングは得意", \t -> do
	itext t 1 "- 再帰的な処理が得意", \t -> do
	text t "* どんなプログラムでもHaskellで書き直せば", \t -> do
	itext t 1 "- その構造が明らかになる", \t -> do
	text t "* 「本質的に向かない仕事」はない"
 ]

cando2 :: Page
cando2 = [\t -> do
	writeTopTitle t "Haskellで何ができるか"
	text t "", \t -> do
	text t "* 発想の転換さえできれば", \t -> do
	itext t 1 "- Haskellは開発効率を大きく向上する", \t -> do
	itext t 1 "- オーダーのレベルでの向上がある、と思う", \t -> do
	text t "* 再利用可能な部品を", \t -> do
	itext t 1 "- 呼吸するくらいに自然に生み出すことができる", \t -> do
	text t "* 関数をさしかえることが簡単なので", \t -> do
	itext t 1 "- 意味を変えずに効率を向上させることが可能", \t -> do
	itext t 1 "- リファクタリングも簡単", \t -> do
	arrowIText t 1 "コードの効率改善や整理はあとからでもできる", \t -> do
	arrowIText t 1 "ルーズな人(僕)に向いている"
 ]

icando :: Page
icando = [\t -> do
	writeTopTitle t "僕にできること"
	text t "", \t -> do
	text t "* Haskellを理解させること", \t -> do
	text t "* Haskellを書けるようにすること", \t -> do
	text t "* それはできる、しかし", \t -> do
	text t "* 魅力を伝えることができるかどうか", \t -> do
	text t "* 人々のモチベーションを高めることができるかどうか", \t -> do
	text t "* 僕はすでにHaskell脳になってしまっている", \t -> do
	text t "* Haskell脳でない人々が", \t -> do
	itext t 1 "- 何を面白く感じるか", \t -> do
	itext t 1 "- 何に不満を持つか", \t -> do
	itext t 1 "- 何を不可解に思うか", \t -> do
	itext t 1 "が、わからない"
 ]

wall :: Page
wall = [\t -> do
	writeTopTitle t "壁"
	text t "", \t -> do
	text t "* Haskellを理解するために越えるべき壁が何か", \t -> do
	text t "* それを知る必要がある", \t -> do
	text t "* 他の言語との比較についても触れたいのだが", \t -> do
	text t "* 十分に比較できるほどに使いこんだ言語がない"
 ]

others :: Page
others = [\t -> do
	writeTopTitle t "Haskell外の世界"
	text t "", \t -> do
	text t "* ここのところずっとHaskellのみにひたってきた", \t -> do
	text t "* それは価値のあることだったが", \t -> do
	text t "* Haskell外の世界を見る必要がある", \t -> do
	text t "* 今回の座談会では、Haskell以外の話もいろいろとしたい", \t -> do
	text t "* そして、最終的にはそれらをHaskellに関連づけたい", \t -> do
	text t "* とにかく、「楽しむこと第一」でやっていきましょう"
 ]

import Lecture

subtitle :: String
subtitle = "第24回 最後に"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	evolution, evolution2, history, more, more2, more3,
	next, next2, tomorrow,
	summary,
	goodbye
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

evolution :: Page
evolution = [\t -> do
	writeTopTitle t "発展途上"
	text t "", \t -> do
	text t "* Haskellは確かに実用的な言語である", \t -> do
	text t "* そのDNAには「発展」という文字が刻まれている", \t -> do
	text t "* もともとのスローガンは", \t -> do
	itext t 1 "\"avoid success at all costs\"", \t -> do
	itext t 1 "「全力で成功を阻止する」", \t -> do
	text t "* 実用的な場面で広く使われてしまうと", \t -> do
	itext t 1 "- コアの部分を変更することが難しくなる", \t -> do
	text t "* 小手先の変更ではなく中心的な部分からの大きな変更を", \t -> do
	itext t 1 "Haskellは歓迎している", \t -> do
	text t "* それによって言語全体の整合性を保ってきた"
 ]

evolution2 :: Page
evolution2 = [\t -> do
	writeTopTitle t "学術的"
	text t "", \t -> do
	text t "* 関数型言語に関する論文の多くは", \t -> do
	itext t 1 "Haskellを例として書かれている", \t -> do
	text t "* Haskellに新しい考えかたが導入されるときには", \t -> do
	itext t 1 "- 実装と論文が同時に作られることが多い", \t -> do
	text t "* 学術と実践とがこんなに近く接している分野はすくない", \t -> do
	text t "* Haskellを学んでいれば", \t -> do
	itext t 1 "- 最新の技術を一早く手に入れることができる"
 ]

history :: Page
history = [\t -> do
	writeTopTitle t "歴史"
	text t "", \t -> do
	text t "* 最新や最先端を追うこともHaskellの楽しみであるが", \t -> do
	text t "* 過去に目を向ければその根は", \t -> do
	itext t 1 "コンピュータが存在する以前までつながっている", \t -> do
	text t "* Haskellの理論の大本であるラムダ計算は", \t -> do
	itext t 1 "- ノイマン式コンピュータよりも古い", \t -> do
	text t "* 本質的に数学的な概念なので陳腐化することがない"
 ]

-- ラムダ計算 1941
-- ノイマン式コンピュータ 1945

more :: Page
more = [\t -> do
	writeTopTitle t "やり残したこと"
	text t "", \t -> do
	text t "* 4日間に凝集した講義なので省いた話題も多い", \t -> do
	text t "* FFIという機能がある", \t -> do
	itext t 1 "- foreign function interfaceの略", \t -> do
	itext t 1 "- C言語の関数をHaskellから使ったり", \t -> do
	itext t 1 "- Haskellの関数をC言語から使ったりできる", \t -> do
	text t "* 関数型言語に特化したデータ構造", \t -> do
	itext t 1 "- 関数型言語では様々なアルゴリズムに樹構造を使う", \t -> do
	itext t 1 "- 効率と堅牢性が両立できることが多い"
 ]

more2 :: Page
more2 = [\t -> do
	writeTopTitle t "やり残したこと"
	text t "", \t -> do
	text t "* 配列について", \t -> do
	itext t 1 "- リストは多くの場面で最適な選択でありえるが", \t -> do
	itext t 1 "- 配列を使ったほうが効率的な場面がある", \t -> do
	text t "* 実行効率の話", \t -> do
	itext t 1 "- ghcには時間や空間のプロファイリング機能がある", \t -> do
	itext t 1 "- 特に遅延評価における空間効率の低下への配慮が必要", \t -> do
	text t "* より先進的、関数型的なIO", \t -> do
	itext t 1 "- 関数型的に、堅牢に入出力することが研究されている", \t -> do
	itext t 1 "- iterateeやFRPといった解が試されている"
 ]

more3 :: Page
more3 = [\t -> do
	writeTopTitle t "やり残したこと"
	text t "", \t -> do
	text t "* パッケージング", \t -> do
	itext t 1 "- cabalというパッケージングの仕組みがある", \t -> do
	itext t 1 "- hackageには多くのパッケージが登録されている", \t -> do
	text t "* その他諸々", \t -> do
	itext t 1 "- 様々なライブラリやパッケージがあり", \t -> do
	itext t 1 "- そのうちのいくつかは非常に本質的な部分で", \t -> do
	itext t 1 "- プログラミングや考え方を効率化する", \t -> do
	itext t 1 "- また、多くの言語拡張があり", \t -> do
	itext t 1 "- それらを使うと、できることが増えたり", \t -> do
	itext t 1 "- 便利になったりする"
 ]

next :: Page
next = [\t -> do
	writeTopTitle t "Haskellの海へ"
	text t "", \t -> do
	text t "* Haskellの海へこぎ出そう", \t -> do
	text t "* この講義で基本的な概念は理解できたので", \t -> do
	text t "* 学者やプログラマ達の様々な成果をむさぼろう"
 ]

next2 :: Page
next2 = [\t -> do
	writeTopTitle t "中級編、上級編"
	text t "", \t -> do
	text t "* より進んだ話題を学びたくなってきたら", \t -> do
	text t "* この講座の中級編、上級編に期待してほしい", \t -> do
	text t "* 現在はまだ準備中だが反響があれば実現させたい", \t -> do
	text t "* また、上級編はより理論や中身の話となるので", \t -> do
	text t "* より実践的なものを学びたい人のために実践編も用意しよう", \t -> do
	text t "* yesodというwebフレームワークがある", \t -> do
	text t "* これについても番外編としてyesod編を作っていきたい"
 ]

tomorrow :: Page
tomorrow = [\t -> do
	writeTopTitle t "明日からの○○プログラミング"
	text t "", \t -> do
	text t "* 明日からの○○プログラミングが変わる", \t -> do
	text t "* 何かを作ろうとしたときにまずはHaskellで書いてみる", \t -> do
	text t "* Haskellで書くということは", \t -> do
	itext t 1 "- 問題の本質を理解するということ", \t -> do
	text t "* 既存のプログラムもHaskellで書き直してみると", \t -> do
	itext t 1 "- 問題の定義がはっきりする", \t -> do
	text t "* 不必要な状態変化を避けるようになる", \t -> do
	arrowIText t 1 "保守管理が容易なコードになる"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Haskellという「考え方」が身についただろうか", \t -> do
	text t "* Haskell的に考えるという姿勢は大事", \t -> do
	text t "* 関数に対する入出力だけを考える", \t -> do
	text t "* 状態変化は極力避ける", \t -> do
	itext t 1 "- それでも必要なら状態変化は隔離する", \t -> do
	text t "* オブジェクト指向で推奨されるやりかたが", \t -> do
	itext t 1 "- 関数型言語では嫌われる場合が多い", \t -> do
	text t "* 問題の本質を見出そう", \t -> do
	text t "* 適切な型を定義しよう"
 ]

goodbye :: Page
goodbye = [\t -> do
	writeTopTitle t "Good luck!"
	text t "", \t -> do
	text t "皆さんの道具箱に新たに仲間入りしたHaskellは", \t -> do
	text t "プログラミングの道具として、そして思考の道具として", \t -> do
	text t "大きな武器となるはずです。", \t -> do
	text t "皆さんが「この世界」をより楽しい場所にしていく助けとして", \t -> do
	text t "Haskellを役立てていってくれたら望外の幸せです。", \t -> do
	text t "明日の世界を今日よりも良いものにしていくために、", \t -> do
	text t "あるいはソースコードを保守可能なものにして", \t -> do
	text t "徹夜するプログラマを減らすために", \t -> do
	text t "Haskellを武器にして戦いましょう。", \t -> do
	text t "では、また会う日まで!"
 ]

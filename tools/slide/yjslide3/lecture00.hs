import Lecture

subtitle :: String
subtitle = "第0回 この講義の意義と目的"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, meaning, meaning2, meaning3,
	meaning4, meaning5,
	haskell
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 受講いただきありがとうございます", \t -> do
	text t "* 期待に応えられるようがんばりますので"
	itext t 5 "よろしくお願いします"
 ]

meaning :: Page
meaning = [\t -> do
	writeTopTitle t "この講義の目指すもの"
	text t "", \t -> do
	text t "* Haskellの基本的な考えかたを理解する", \t -> do
	text t "* Haskellを書けるようにする", \t -> do
	dvArrowShort t
	text t "* 「明日からのXXプログラミングが変わる」", \t -> do
	itext t 1 "XXはなじみの言語に置き換えてください", \t -> do
	text t "* Haskellという武器を手に入れる", \t -> do
	itext t 1 "使えるところにはHaskellを使おう"
 ]

meaning2 :: Page
meaning2 = [\t -> do
	writeTopTitle t "XX言語のほうが普及してるけど?"
	text t "", \t -> do
	text t "* Haskellには優れた特徴が多くある", \t -> do
	text t "* それらの特徴により以下が望める", \t -> do
	itext t 1 "- 開発効率の向上", \t -> do
	itext t 1 "- 保守作業の効率化", \t -> do
	text t "* Haskellを使わなくても", \t -> do
	itext t 1 "- プログラミングにおける良い習慣が身につく", \t -> do
	text t "* 普及している言語を使える人は数多くいる", \t -> do
	text t "* よって、それだけに精通したとしても", \t -> do
	arrowIText t 1 "コモディティ化してしまう恐れがある", \t -> do
	itext t 1 "「君の代わりなどいくらでもいるのだよ」"
 ]

meaning3 :: Page
meaning3 = [\t -> do
	writeTopTitle t "Haskellが使えます"
	text t "", \t -> do
	text t "* Haskellを使えるということは", \t -> do
	itext t 1 "- 優秀な人材である可能性が高い", \t -> do
	itext t 1 "- プログラミングが好きで自発的に学んでいる", \t -> do
	itext t 1 "- 保守可能なプログラムを書ける", \t -> do
	itext t 2 "と思われる、かもしれない", \t -> do
	text t "* Tsuru Capital(www.tsurucapital.com)という実例", \t -> do
	itext t 0.6 "「Haskellを社内の標準言語にすれば、"
	itext t 1 "優秀な人材を集められる」"
 ]

meaning4 :: Page
meaning4 = [\t -> do
	writeTopTitle t "僕はコモディティじゃない!"
	text t "", \t -> do
	text t "* あなたが今持っている技術は会社や社会にとって", \t -> do
	itext t 1 "「かけがえのないもの」だろうか?", \t -> do
	text t "* 今持っている技術にHaskellをプラスすることで", \t -> do
	itext t 1 "「取り換え不可能な人材」になれる", \t -> do
	text t "* 会社や社会にとって「取り換え不可能」な人材になれば", \t -> do
	itext t 1 "- 多少のわがままも言える", \t -> do
	itext t 1 "- 「自分らしく」生きることができる"
 ]

meaning5 :: Page
meaning5 = [\t -> do
	writeTopTitle t "明日の仕事、未来の自分"
	text t "", \t -> do
	text t "* 明日の仕事に役立つ", \t -> do
	text t "* 未来の自分の武器になる", \t -> do
	text t "* それだけじゃない!", \t -> do
	text t "* 何よりも、「楽しい」", \t -> do
	text t "* Haskellの魅力", \t -> do
	itext t 1 "- 純粋で美しい", \t -> do
	itext t 1 "- 他の言語との距離", \t -> do
	itext t 1 "- アカデミックな世界とのつながり", \t -> do
	itext t 1 "- 実用性"
 ]

haskell :: Page
haskell = [\t -> do
	writeTopTitle t "名前の由来"
	text t "", \t -> do
	writeImageRight t haskellBCurry, \t -> do
	text t "* Haskell Brooks Curry"
	itext t 1 "(1900.9.12 - 1982.9.1)", \t -> do
	text t "* コンビネータ論理の研究をした"
	itext t 1 "数学者・論理学者", \t -> do
	text t "* コンビネータ論理は関数型言語の"
	itext t 1 "基盤となっている"
 ]

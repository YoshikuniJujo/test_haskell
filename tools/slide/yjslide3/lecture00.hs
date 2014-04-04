import Lecture
import System.Random
import System.IO.Unsafe
import Control.Monad

subtitle :: String
subtitle = "第0回 この講義の意義と目的"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, meaning, meaning2, meaning3,
	meaning4, meaning5,
	haskell, haskell2, haskell3, haskell4,
	lecture, attention, lecture2,
	lecture3, lecture4, lecture4_5, lecture4_7, lecture5, lecture6,
	summary
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
	itext t 1 "使えるところにはHaskellを使う"
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
	text t "* よって、それ「だけ」に精通したとしても", \t -> do
	arrowIText t 1 "コモディティ化してしまう恐れがある"
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
	text t "* 今持っている技術は10年後、20年後にも通用するだろうか?", \t -> do
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
	text t "* Haskellを学ぶということは", \t -> do
	itext t 1 "- 「考え方」を学ぶということ", \t -> do
	itext t 1 "- アルゴリズムやロジックは陳腐化しない", \t -> do
	text t "* それだけじゃない!", \t -> do
	text t "* 何よりも「楽しい」", \t -> do
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

haskell2 :: Page
haskell2 = [\t -> do
	writeTopTitle t "歴史"
	text t "", \t -> do
	text t "* 1987年 遅延関数型言語の統合の必要性が議決された", \t -> do
	itext t 1 "- 遅延評価する純粋関数型言語の乱立", \t -> do
	itext t 1 "- 当時12以上存在していた", \t -> do
	itext t 1 "- オープンな標準を作成するための委員会の発足", \t -> do
	text t "* 1990年 Haskell 1.0", \t -> do
	itext t 1 "- 言語仕様が策定された", \t -> do
	text t "* 1998年 Haskell 98", \t -> do
	itext t 1 "- 言語仕様と基本ライブラリの定義", \t -> do
	itext t 1 "- The Haskell 98 Reportとして発表"
 ]

haskell3 :: Page
haskell3 = [\t -> do
	writeTopTitle t "歴史"
	text t "", \t -> do
	text t "* 2003年 Haskell 98 の改定", \t -> do
	itext t 1 "- Haskell 98 Language and Libraries:"
	itext t 2 "The Revised Report", \t -> do
	text t "* 2006年 Haskell' (Haskell Prime)", \t -> do
	itext t 1 "- Haskell 98のマイナーバージョンアップ", \t -> do
	text t "* 2010年 Haskell 2010", \t -> do
	itext t 1 "- 他の言語とのバインディング(FFI)", \t -> do
	itext t 1 "- モジュールの階層構造", \t -> do
	itext t 1 "- 等々が取り入れられた"
 ]

haskell4 :: Page
haskell4 = [\t -> do
	writeTopTitle t "処理系"
	text t "", \t -> do
	text t "* 「Haskell」とは仕様の名前", \t -> do
	itext t 1 "- 多くの言語とは異なり処理系の前に仕様がある", \t -> do
	text t "* 処理系には以下のものがある", \t -> do
	itext t 1 "GHC, Hugs, Gofer, iHBC, Helium, jhc, nhc98", \t -> do
	text t "* GHCとはGlasgow Haskell Compilerの略", \t -> do
	text t "* 現在よく使われているのはGHC", \t -> do
	text t "* GHCにはHaskell 2010にない拡張機能が含まれる", \t -> do
	itext t 1 "- 明示的に宣言することで使えるようになる", \t -> do
	itext t 1 "- 十分に吟味されたものは次の標準に取り込まれるかも", \t -> do
	text t "* この講義はGHCを使って進めていく"
 ]

lecture :: Page
lecture = [\t -> do
	writeTopTitle t "講義の進めかた"
	text t "", \t -> do
	text t "* 主に対話的環境を使う", \t -> do
	text t "* 式を打ち込んでその結果を得る", \t -> do
	text t "* 入出力を学んだらコンパイル実行について見る", \t -> do
	text t "* GHCはコンパイルせずにインタプリタとして使うこともできる"
 ]

attention :: Page
attention = [\t -> do
	writeTopTitle t "ターミナル"
	text t "", \t -> do
	text t "* この講義のなかではターミナルを主に利用する", \t -> do
	text t "* HaskellのIDEもあるようだが", \t -> do
	itext t 1 "- 演者が使ったことがない", \t -> do
	itext t 1 "- ターミナルを使う方法のほうが応用が利く", \t -> do
	itext t 1 "- つまり、後からIDEを使う方法を学ぶこともできる"
 ]

lecture2 :: Page
lecture2 = [\t -> do
	writeTopTitle t "手を動かしてみる"
	text t "", \t -> do
	text t "* ghcに触れてみよう", \t -> do
	text t "* まずはバージョンの確認をしてみよう", \t -> do
	itext t 1 "% ghc --version", \t -> do
	itext t 1 "The Glorious Glasgow Haskell Compilation System,"
	itext t 2 "version 7.6.3", \t -> do
	text t "* 次に対話環境を立ち上げてみよう(ghciの'i'を忘れずに!)", \t -> do
	itext t 1 "% ghci", \t -> do
	itext t 1 "(数行のメッセージ)"
	itext t 1 "Prelude>", \t -> do
	text t "* プロンプトが出た", \t -> do
	itext t 1 "- \"Prelude\"の意味はそのうち明らかになる"
 ]

lecture3 :: Page
lecture3 = [\t -> do
	writeTopTitle t "手を動かしてみる"
	text t "", \t -> do
	text t "Prelude>", \t -> do
	itext t 1 "- 入力を促(prompt)されている", \t -> do
	itext t 1 "- しかし、何を入力すればいいのだろうか?", \t -> do
	itext t 1 "- 困ったのでとりあえず対話を終了させたい", \t -> do
	text t "Prelude> :quit", \t -> do
	text t "% ", \t -> do
	itext t 1 "- 出られた", \t -> do
	itext t 1 "- 出られることがわかったので安心してもう一回", \t -> do
	text t "% ghci", \t -> do
	text t "Prelude>", \t -> do
	itext t 1 "- いつでも出られるのでゆっくり考えよう"
 ]

l4Number :: Int
l4Number = unsafePerformIO $ randomRIO (0, 100)
l4Char :: Char
l4Char = unsafePerformIO $ randomRIO ('a', 'z')

lecture4 :: Page
lecture4 = [\t -> do
	writeTopTitle t "手を動かしてみる"
	text t "", \t -> do
	text t "Prelude>", \t -> do
	itext t 1 "- とりあえず数字を入れてみよう", \t -> do
	text t $ "Prelude> " ++ show l4Number, \t -> do
	text t $ show l4Number
	text t $ "Prelude> ", \t -> do
	itext t 1 "- 数字を入れるとそのまま表示される", \t -> do
	itext t 1 "- 終了するまで次々に入力を促(prompt)される", \t -> do
	itext t 1 "- 文字を入れてみよう", \t -> do
	text t $ "Prelude> " ++ show l4Char, \t -> do
	text t $ show l4Char
 ]

lecture4_5 :: Page
lecture4_5 = [\t -> do
	writeTopTitle t "手を動かしてみる"
	text t "", \t -> do
	text t "* 今回用意したキーボードは", \t -> do
	itext t 1 "英語配列のHappy Hacking Keyboard", \t -> do
	text t "* 熟練すると使いやすくなるキーボードだ", \t -> do
	text t "* すこしクセがある", \t -> do
	text t "* バックスペースを入力したいときには", \t -> do
	itext t 1 "Fn + Delete", \t -> do
	text t "* 試してみよう", \t -> do
	text t "Prelude> miss", \t -> do
	itext t 1 "Fn + Deleteを4回入力する", \t -> do
	text t "Prelude>"
 ]

lecture4_7 :: Page
lecture4_7 = [\t -> do
	writeTopTitle t "手を動かしてみる"
	text t "", \t -> do
	text t "* バックスペースの代わりに以下のキーも使える", \t -> do
	itext t 1 "Ctrl + h", \t -> do
	text t "* こちらのほうが打ちやすいかもしれない", \t -> do
	text t "* 試してみよう", \t -> do
	text t "Prelude> machigaeta", \t -> do
	itext t 1 "Ctrl + hを10回入力する", \t -> do
	text t "Prelude>"
 ]

l5Number1, l5Number2 :: Int
[l5Number1, l5Number2] =
	unsafePerformIO $ replicateM 2 $ randomRIO (0, 100)

lecture5 :: Page
lecture5 = [\t -> do
	writeTopTitle t "手を動かしてみる"
	text t "", \t -> do
	text t "Prelude>", \t -> do
	itext t 1 "- 文字列を入れてみよう", \t -> do
	text t "Prelude> \"Hello, world!\"", \t -> do
	text t "\"Hello, world!\"", \t -> do
	itext t 1 "- 対話環境で伝統的な挨拶ができた", \t -> do
	itext t 1 "- 計算をしてみよう", \t -> do
	text t $ "Prelude> " ++ show l5Number1 ++ " + " ++ show l5Number2, \t -> do
	text t $ show $ l5Number1 + l5Number2
 ]

l6Number1, l6Number2, l6Number3, l6Number4 :: Int
[l6Number1, l6Number2, l6Number3, l6Number4] =
	unsafePerformIO $ replicateM 4 $ randomRIO (0, 100)

lecture6 :: Page
lecture6 = [\t -> do
	writeTopTitle t "手を動かしてみる"
	text t "", \t -> do
	itext t 1 "- 引き算とかけ算もしてみよう", \t -> do
	text t $ "Prelude> " ++ show l6Number1 ++ " - " ++ show l6Number2, \t -> do
	text t $ show $ l6Number1 - l6Number2, \t -> do
	text t $ "Prelude> " ++ show l6Number3 ++ " * " ++ show l6Number4, \t -> do
	text t $ show $ l6Number3 * l6Number4, \t -> do
	text t ""
	text t "* Haskellは電卓として使えることがわかった", \t -> do
	text t "* 今回はghciにとりあえず触れてみた", \t -> do
	text t "* 次の講義で実質的な内容に触れていこう"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Haskellの持つ様々な特徴によって", \t -> do
	itext t 1 "- 開発や保守作業の効率化が期待できる", \t -> do
	itext t 1 "- 他の言語を使う場合でも役に立つ", \t -> do
	text t "* Haskellを使えれば脱「コモディティ」が望める", \t -> do
	text t "* Haskellを学ぶのは楽しい", \t -> do
	text t "* Haskellの歴史について簡単に触れた", \t -> do
	text t "* ghcの対話環境を紹介した"
 ]

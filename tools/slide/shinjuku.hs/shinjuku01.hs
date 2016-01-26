import Lecture

subtitle :: String
subtitle = "0. 挨拶等"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, history, myself, myProject,
	yjhs, hkh, aboutThis, plan
 ]

prelude :: Page
prelude = [ \t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* shinjuku.hsに参加いただきありがとうございます", \t -> do
	text t "* @YoshikuniJujoです", \t -> do
	text t "* 特技はHaskell愛です", \t -> do
	text t "* Haskell愛を共有(強要?)できる場に感謝しています", \t -> do
	text t "* @threetreeslightさんと@kuranari_tmさんのおかげです", \t -> do
	text t "* あらためて「ありがとうございます」", \t -> do
	text t "* @hase-2015さん、Akira Osadaさん、@igrepさん", \t -> do
	itext t 1 "ご参加ありがとうございます", \t -> do
	text t "* @yuyhirakaさんは今日はいらしていませんが", \t -> do
	itext t 1 "コミュニティへのご参加ありがとうございます", \t -> do
	arrowIText t 2 "あとでメールしておきます(^_^;"
	]

history :: Page
history = [ \t -> do
	writeTopTitle t "shinjuku.hsへの経緯"
	text t "", \t -> do
	text t "* CodeIQで問題を解いた", \t -> do
	text t "* SideCIの@sumyappさんに声をかけていただいた", \t -> do
	text t "* 会って話をする、@threetreeslightさんに紹介してもらう", \t -> do
	text t "* Haskellを教えはじめる", \t -> do
	text t "* @kuranari_tmさん参加", \t -> do
	text t "* DoorKeeperで公開", \t -> do
	text t "* 第1回 shinjuku.hs <- イマココ", \t -> do
	text t "* 日本でHaskell大流行"
	]

myself :: Page
myself = [ \t -> do
	writeTopTitle t "@YoshikuniJujoの基本データ"
	text t "", \t -> do
	text t "* OS: Gentoo on Gentoo (Gentoo上のKVM上にGentooを)", \t -> do
	text t "* 言語: 日本語、Haskell", \t -> do
	text t "* GUI: xmonad", \t -> do
	text t "* ターミナルエミュレータ: rxvt-unicode, tmux", \t -> do
	text t "* シェル: zsh", \t -> do
	text t "* エディタ: vim", \t -> do
	text t "* 日本語入力: TUT-Code (漢字直接入力)", \t -> do
	text t "* ブラウザ: firefox, vimperator", \t -> do
	text t "* メールクライアント: mutt", \t -> do
	text t "* キーボード: Happy Hacking Keyboard"
	]

myProject :: Page
myProject = [ \t -> do
	writeTopTitle t "Hackageに置いてあるもの"
	text t "", \t -> do
	text t "* peyotls: TLSライブラリ", \t -> do
	text t "* tighttp: HTTPライブラリ", \t -> do
	text t "* papillon: PEGパーサジェネレータ", \t -> do
	text t "* cabal2ebuild: .cabalから.ebuildを生成", \t -> do
	text t "* hssourceinfo: コード行数やモジュールの依存関係を表示", \t -> do
	text t "* markdown2svg: MarkdownからSVGを生成", \t -> do
	text t "* simple-pipe: Conduitをシンプルにしたもの", \t -> do
	text t "* simplest-sqlite: SQLiteのC関数の薄いラッパ", \t -> do
	text t "* zot: 難解プログラミング言語zotのインタプリタ", \t -> do
	text t "* xturtle: タートルグラフィック", \t -> do
	text t "* その他多数"
	]

yjhs :: Page
yjhs = [ \t -> do
	writeTopTitle t "@YoshikuniJujoとHaskell"
	text t "", \t -> do
	text t "* 2002年「Unix User」2月号でRubyに出会う", \t -> do
	itext t 1 "+ ちなみにGentooとの出会いもこれ", \t -> do
	text t "* RubyかわいいよRuby", \t -> do
	text t "* 「Rubyコード完全解説」に", \t -> do
	itext t 1 "「Haskellや...ではないので一般人にも読める」", \t -> do
	itext t 1 "(http://i.loveruby.net/ja/rhg/book/intro.html)", \t -> do
	arrowIText t 1 "「一般人に読めない」Haskellって一体?", \t -> do
	text t "* 興味を持ち「The Craft of Functional Programming」購入", \t -> do
	itext t 1 "+ これが2003年1月1日", \t -> do
	text t "* それからはや13年間Haskellにはまり続ける"
	]

hkh :: Page
hkh = [ \t -> do
	writeTopTitle t "HaskellかわいいよHaskell"
	text t "", \t -> do
	text t "* Rubyからの乗りかえでそのころ決定的だったことは実は", \t -> do
	itext t 1 "+ Rubyの演算子の考えかた", \t -> do
	itext t 1 "+ 演算子は左側のオブジェクトに属するメソッド", \t -> do
	itext t 1 "+ 演算子の左と右の値は「対等」なはずなのに", \t -> do
	itext t 1 "+ 「オブジェクト志向」は「第1引数」を優遇しすぎ", \t -> do
	arrowIText t 1 "美しくない", \t -> do
	text t "* そして", \t -> do
	arrowIText t 1 "知れば知るほど美しいHaskell", \t -> do
	arrowIText t 1 "13年使っていても日々新しい概念を学んでいる"
	]

aboutThis :: Page
aboutThis = [ \t -> do
	writeTopTitle t "このスライドについて"
	text t "", \t -> do
	text t "* ちなみにこのスライドはHaskell製です", \t -> do
	itext t 1 "(% view shinjuku01.hs)", \t -> do
	text t "* xturtleというパッケージを使っています", \t -> do
	text t "* いわゆるタートルグラフィックのHaskell版です", \t -> do
	text t "* ステップバイステップでお絵描きができるので便利", \t -> do
	text t "* クリックとかのイベントにも反応するので", \t -> do
	itext t 1 "ちょっとしたGUIプログラミングもできなくもない", \t -> do
	text t "* 対話的に使えるように工夫したぶん", \t -> do
	itext t 1 "+ かなり手続き的なコード", \t -> do
	itext t 1 "+ 移植性が悪い(Xlibを直接使っている)", \t -> do
	itext t 1 "(対話環境で対話的にSVGファイルを作成するデモ)"
	]

plan :: Page
plan = [ \t -> do
	writeTopTitle t "どのように進めていくか"
	text t "", \t -> do
	text t "* 初心者に優しい", \t -> do
	text t "* 中上級者をあきさせない", \t -> do
	text t "* できるだけ、ごまかさずに教えたい", \t -> do
	text t "* これらを両立させたい", \t -> do
--	text t "* さっきのxturtleのデモはいいバランスだと思う"
	text t "* 本質から教えていく", \t -> do
	text t "* IOモナドをきちんと理解する最短経路を通る", \t -> do
	text t "* 中上級者をあきさせないためにスピードを保つ", \t -> do
	text t "* 初心者にはSlackにて個別指導をさせていただきます"
	]

_myself :: Page
_myself = [ \t -> do
	writeTopTitle t "@YoshikuniJujoとは"
	text t "", \t -> do
	text t "* ここで話しているのは誰なのか", \t -> do
	text t "* 会社という肩書きがないので", \t -> do
	itext t 1 "CodeEvalでAll TimeでHaskellで現在1位", \t -> do
	itext t 1 "いちおうPerl 6のcontributor", \t -> do
	arrowIText t 2 "Pugs / 11 ++ / 6 --", \t -> do
	itext t 1 "hackageにアップロードしたパッケージ多数", \t -> do
	itext t 1 "(https://hackage.haskell.org/user/YoshikuniJujo)", \t -> do
	text t "* その他Haskellのコードをいろいろと書いてます", \t -> do
	text t "* Webページも鋭意作成中", \t -> do
	itext t 1 "(https://skami.iocikun.jp)"
	]

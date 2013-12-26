import Lecture

subtitle :: String
subtitle = "Haskellの楽しみ"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	[flip writeTitle subtitle], languages, perl, perl2, cxx, php,
	javascript, ugly,
	languages2
 ]

languages :: Page
languages = [\t -> do
	writeTopTitle t "いろいろな言語"
	text t "", \t -> do
	text t "* 世のなかにはいろいろな言語がある", \t -> do
	text t "* それぞれに美しさがあり", \t -> do
	text t "* それぞれに醜さがある", \t -> do
	text t "* 美しさと醜さがうまくからまりあい魅力となる", \t -> do
	text t "* 醜いとされる言語には以下のものがある", \t -> do
	itext t 1 "Perl, C++, PHP, Javascript", \t -> do
	text t "* ネットサーフィンのなかで得た主観的な結論である", \t -> do
	text t "* それぞれについて、主観的な寸評をしてみよう"
 ]

perl :: Page
perl = [\t -> do
	writeTopTitle t "Perl"
	text t "", \t -> do
	text t "* 個人的には、はじめて学んだ言語", \t -> do
	text t "* 掲示板とアクセスカウンターが作りたかった", \t -> do
	text t "* いろいろと変態的なところのある言語", \t -> do
	text t "* 引数から得たファイルまたは標準入力の内容を表示", \t -> do
	itext t 1 "while (<>) { print }", \t -> do
	text t "* えっっ?", \t -> do
	text t "* さらに、", \t -> do
	itext t 1 "- 最初の一行を表示: $line = <>; print $line", \t -> do
	itext t 1 "- 全部を表示: @lines = <>; print @lines", \t -> do
	text t "* えっっ?"
 ]

perl2 :: Page
perl2 = [\t -> do
	writeTopTitle t "Perl"
	text t "", \t -> do
	text t "* いろいろと変態的", \t -> do
	text t "* そこが魅力とも言える", \t -> do
	text t "* Unix文化との強い結びつきがある", \t -> do
	text t "* 汎用プログラミング言語よりもシェルスクリプトに近い", \t -> do
	text t "* 用途が広がるにつれていろいろとごてごてしてきた", \t -> do
	text t "* 「よくやること」を簡潔に書ける", \t -> do
	text t "* 古き良きハッカーのための言語"
 ]

cxx :: Page
cxx = [\t -> do
	writeTopTitle t "C++"
	text t "", \t -> do
	text t "* 「この門をくぐる者は一切の希望を捨てよ」", \t -> do
	text t "* 僕自身は門をくぐらなかった", \t -> do
	text t "* 「効率を犠牲にせずすべての要望に答える」がコンセプト", \t -> do
	text t "* 一言で言えば無茶", \t -> do
	text t "* 「みんなに使ってもらいたい」という気持ちが強すぎた", \t -> do
	text t "* 何でもある", \t -> do
	text t "* それぞれが自分のお気に入りの片隅で生活しているらしい", \t -> do
	text t "* 言語設計のなかに恣意的な選択が多すぎる", \t -> do
	itext t 1 "- 選択に必然性がない", \t -> do
	itext t 1 "- 「どちらでも良いが、とりあえずこっち」感", \t -> do
	itext t 1 "- 根本的な設計ミスが感じられる"
 ]

php :: Page
php = [\t -> do
	writeTopTitle t "PHP"
	text t "", \t -> do
	text t "* よくわからないが、みんなが「ダメ」と言っている", \t -> do
	text t "* ウェブ上のドキュメントは豊富", \t -> do
	text t "* HTMLに組み込むとちょっと面白いことができる言語", \t -> do
	text t "* 本格的なプログラムを組むためのものではない", \t -> do
	itext t 1 "...らしい"
 ]

javascript :: Page
javascript = [\t -> do
	writeTopTitle t "Javascript"
	text t "", \t -> do
	text t "* プロトタイプベースのオブジェクト指向を採用", \t -> do
	text t "* 非常にシンプルなやりかたでオブジェクト指向を実現", \t -> do
	text t "* 基本的なコンセプトが美しい", \t -> do
	text t "* 関数型プログラミングも無理なく可能", \t -> do
	text t "* 本質的には美しい言語だと思う、が", \t -> do
	text t "* 基本的な部分がシンプルすぎるために", \t -> do
	text t "* 内臓が見えてしまう系の醜さがあるのかもしれない", \t -> do
	text t "* いずれにしても、醜さがわかるほどに使ったことがない"
 ]

ugly :: Page
ugly = [\t -> do
	writeTopTitle t "醜いとされる言語(まとめ)"
	text t "", \t -> do
	text t "* PHP以外はどれもそれなりに魅力的", \t -> do
	text t "* PHPからはとくに新しい概念を学べる気がしない", \t -> do
	text t "* Perlは自然言語的で、スラング的な面白さがある", \t -> do
	text t "* Javascriptは小さな核でどこまでできるかを示す", \t -> do
	text t "* C++は言語の実験場"
 ]

languages2 :: Page
languages2 = [\t -> do
	writeTopTitle t "いろいろな言語"
	text t "", \t -> do
	text t "* 僕が美しいと思う言語がいくつかある", \t -> do
	itext t 1 "C, Ruby, Scheme, Haskell", \t -> do
	text t "* C言語はチューリングマシンのプログラムを書く道具", \t -> do
	text t "* Rubyは最も成功したlisp", \t -> do
	text t "* 美しく使いやすい形でlispにオブジェクト指向を導入", \t -> do
	text t "* マクロを捨てることで括弧から逃れることに成功", \t -> do
	text t "* 実用的なもののなかでSchemeは最も単純なlisp", \t -> do
	text t "* lispはラムダ計算を書く道具", \t -> do
	text t "* Haskellは型付きラムダ計算を書く道具"
 ]

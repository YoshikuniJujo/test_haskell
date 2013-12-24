import Lecture

subtitle :: String
subtitle = "第46回 パッケージ化"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	simplest, simplest2, simplest3, simplest4, simplest5, simplest6,
	simplest7, simplest8, simplest9, simplest10, simplest11, simplest12,
	simplest13, simplest14
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ライブラリやアプリケーションを作ったとき", \t -> do
	text t "* それを簡単にパッケージ化できる仕組みが欲しい", \t -> do
	text t "* HaskellにはCabalがある", \t -> do
	text t "* CabalはHaskellに特化した高機能makeと考えられる", \t -> do
	text t "* Cabalはcabal-installを使うとより簡単に扱える"
 ]

simplest :: Page
simplest = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* まずは単純な例を見てみよう", \t -> do
	text t "* その前に簡単なツールをインストールしておく", \t -> do
	text t "* cabal install cabal-dir", \t -> do
	text t "* Cabalを使ってインストールする前に", \t -> do
	itext t 1 "- インストール先のディレクトリを調べられる", \t -> do
	text t "* インストール先のディレクトリにパスを通しておく", \t -> do
	text t "* 典型的には~/.cabal/binあたりになる"
 ]

simplest2 :: Page
simplest2 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* サンプルコード", \t -> do
	itext t 1 "% cat sample.hs"
	itext t 1 "main :: IO ()"
	itext t 1 "main = putStrLn \"sample\""
 ]

simplest3 :: Page
simplest3 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* Cabalはライブラリを提供している", \t -> do
	text t "* それを利用する単純なセットアップコードを用意する", \t -> do
	text t "* そのコードが.cabalファイルを読み込むという仕組み"
	text t "", \t -> do
	text t "* セットアップコードを用意しよう", \t -> do
	itext t 1 "% cat Setup.hs"
	itext t 1 "import Distribution.Simple; main = defaultMain", \t -> do
	text t "* Distribution.SimpleモジュールのdefaultMainを実行する", \t -> do
	text t "* ほとんどの場合、これだけで十分"
 ]

simplest4 :: Page
simplest4 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* .cabalファイルを以下のようにする", \t -> do
	itext t 1 "% cat sample.cabal"
	itext t 1 "cabal-version: >= 1.2"
	itext t 1 "build-type: Simple"
	itext t 1 ""
	itext t 1 "name: sample"
	itext t 1 "version: 0.1.0.0"
	itext t 1 ""
	itext t 1 "executable sample"
	itext t 2 "main-is: sample.hs"
	itext t 2 "build-depends: base == 4.*"
 ]

simplest5 :: Page
simplest5 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "cabal-version: >= 1.2", \t -> do
	itext t 1 "Cabalのバージョンを指定する", \t -> do
	itext t 1 "バージョン1.2で.cabalファイルの記法が大きく変化", \t -> do
	itext t 1 "よって特別な理由がなければここは>= 1.2以上とする", \t -> do
	itext t 1 "後述のsource-repositoryが必要なら>= 1.6", \t -> do
	text t "build-type: Simple", \t -> do
	itext t 1 "Setup.hsがdefaultMainのみであることを示す", \t -> do
	text t "name: sample", \t -> do
	itext t 1 "パッケージ名を指定する", \t -> do
	text t "version: 0.1.0.0", \t -> do
	itext t 1 "パッケージのバージョンを指定する"
 ]

simplest6 :: Page
simplest6 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* パッケージのバージョンについてHaskellの慣習がある", \t -> do
	text t "* バージョンはx.y.z.wという4桁で表し", \t -> do
	itext t 1 "- 機能(API)に変化のないバグ修正等はwを増やし", \t -> do
	itext t 1 "- 互換性のある機能追加の場合はzを増やし", \t -> do
	itext t 1 "- 互換性のない変更の場合にはxかyを増やす", \t -> do
	text t "* ただし、個人的には実験的なバージョンでは", \t -> do
	itext t 1 "- 互換性のない変更が多くなるので、", \t -> do
	itext t 1 "- 0.0.1.wとして、wを上げていったほうが", \t -> do
	itext t 1 "- バージョンが上がり過ぎなくて良いようにも思う"
 ]

simplest7 :: Page
simplest7 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "executable sample", \t -> do
	itext t 1 "ここから実行可能ファイルのセクションが始まる", \t -> do
	itext t 1 "sampleという実行可能ファイルを作ることを示す", \t -> do
	text t "main-is: sample.hs", \t -> do
	itext t 1 "Mainモジュールを定義したファイル名を示す", \t -> do
	text t "build-depends: base == 4.*", \t -> do
	itext t 1 "実行可能ファイルに必要な他のパッケージを指定する", \t -> do
	itext t 1 "baseはPreludeモジュールを含むのでほぼ必須", \t -> do
	itext t 1 "またパッケージのバージョンの範囲も指定する", \t -> do
	itext t 1 "4.x.y.zが指定されている"
 ]

simplest8 :: Page
simplest8 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* .cabalファイルではインデントにタブを使えない", \t -> do
	text t "* 不便だが宗教戦争を避けたということだろうか", \t -> do
	text t "* タブは空白8文字分とすれば良いと思うが", \t -> do
	itext t 1 "- 異論が多いということだろう"
 ]

simplest9 :: Page
simplest9 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* まずはSetup.hsをコンパイルしておく", \t -> do
	itext t 1 "% ghc Setup.hs -o setup", \t -> do
	text t "* 設定の読み込み", \t -> do
	itext t 1 "% ./setup configure", \t -> do
	text t "* ビルド", \t -> do
	itext t 1 "% ./setup build", \t -> do
	text t "* ビルドされた実行ファイルは./dist以下に置かれる", \t -> do
	itext t 1 "% ./dist/build/sample/sample"
	itext t 1 "sample"
 ]

simplest10 :: Page
simplest10 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* ここでインストール先のディレクトリを見てみる", \t -> do
	itext t 1 "% cabal-dir"
	itext t 1 "prefix : /usr/local/"
	itext t 1 "bindir : bin"
	itext t 1 "libdir : lib/sample-0.1.0.0/ghc-7.6.3"
	itext t 1 "datadir: share/sample-0.1.0.0"
	itext t 1 "htmldir: share/doc/sample-0.1.0.0/html", \t -> do
	text t "* この結果は環境によって変わる", \t -> do
	text t "* 今回のsampleは実行ファイルのみなのでbindirを見る", \t -> do
	text t "* prefixと合わせて/usr/local/bin/がインストール先となる"
 ]

simplest11 :: Page
simplest11 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* ユーザーにローカルなディレクトリにする", \t -> do
	itext t 1 "% ./setup configure --user", \t -> do
	itext t 1 "% cabal-dir"
	itext t 1 "prefix : /home/user-name/.cabal/"
	itext t 1 "...", \t -> do
	text t "* configureに--userフラグをつけると", \t -> do
	itext t 1 "- ユーザーごとのディレクトリがインストール先となる", \t -> do
	text t "* --userフラグの反対が--globalフラグ", \t -> do
	itext t 1 "- --globalフラグはデフォルトの動作"
 ]

simplest12 :: Page
simplest12 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* prefixを変更してみる", \t -> do
	itext t 1 "% ./setup configure --prefix=/usr"
	itext t 1 "% cabal-dir"
	itext t 1 "prefix : /usr/"
	itext t 1 "bindir : bin"
	itext t 1 "...", \t -> do
	text t "* こうするとsampleは/usr/bin/下にインストールされる"
 ]

simplest13 :: Page
simplest13 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* bindirを変更してみる", \t -> do
	itext t 1 "% ./setup configure --prefix=$HOME/test"
	itext t 4 "--bindir='$prefix/binary'"
	itext t 1 "% cabal-dir"
	itext t 1 "prefix : /home/user-name/test/"
	itext t 1 "bindir : binary"
	itext t 1 "...", \t -> do
	text t "* ここらへんで実際にインストールをしてみよう", \t -> do
	itext t 1 "% ./setup copy"
	itext t 1 "% ~/test/binary/sample"
	itext t 1 "sample"
 ]

simplest14 :: Page
simplest14 = [\t -> do
	writeTopTitle t "単純な例"
	text t "", \t -> do
	text t "* $prefix変数が使える", \t -> do
	itext t 1 "--bindir='$prefix/binary'", \t -> do
	itext t 1 "- $prefixはprefixの値を参照する", \t -> do
	text t "* その他、以下の変数が使える", \t -> do
	itext t 1 "- $pkgid, $pkg, $version, $compiler, $os, $arch"
 ]

someXX :: Page
someXX = [\t -> do
	writeTopTitle t "使える変数"
	text t "", \t -> do
	text t "$prefix, $pkgid, $pkg, $version, $compiler, $os, $arch"
 ]

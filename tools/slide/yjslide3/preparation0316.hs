import Lecture

subtitle :: String
subtitle = "事前準備 3/16"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, slide,
	installGit, tryGitBash, installGHC,
--	installMinGW, installMSYS,
--	installWxHaskell
	installWxPack,
	installNano, installEmacs,
	testSome,
	others
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* ついに、4月からトライアルの講義が始まる", \t -> do
	text t "* 考えられる問題点にはできるだけ対処しておきたい", \t -> do
	text t "* 予測できる問題にすべて対処したとしても", \t -> do
	itext t 1 "- 予測できない問題がポロポロと出てくるものだから", \t -> do
	text t "* 今回の「準備」のメインは", \t -> do
	itext t 1 "- 受講生用のPCの環境を整えること", \t -> do
	text t "* インストールするべきソフトウェアは", \t -> do
	itext t 1 "git, ghc, wxWidgets, wxHaskell", \t -> do
	text t "* gitをインストールする際に以下も多分インストールできる", \t -> do
	itext t 1 "git bash, vim", \t -> do
	text t "* 時間があれば以下のエディタもインストールしたい", \t -> do
	itext t 1 "nano, emacs"
 ]

slide :: Page
slide = [\t -> do
	writeTopTitle t "スライドの上映テスト"
	text t "", \t -> do
	text t "* 使用するPC(zenbook)の外部モニタ用の端子は", \t -> do
	itext t 1 "- mini VGA", \t -> do
	itext t 1 "- micro HDMI", \t -> do
	text t "* 用意したケーブルは", \t -> do
	itext t 1 "- micro HDMI - VGA", \t -> do
	itext t 1 "- micro HDMI - DVI", \t -> do
	text t "* PCのモニタへの出力はテスト済み", \t -> do
	itext t 1 "- ただし横長になってしまう", \t -> do
	itext t 1 "- 映写機では問題ないと予想される"
 ]

installGit :: Page
installGit = [\t -> do
	writeTopTitle t "gitのインストール", \t -> do
	text t "* 以下のサイトから", \t -> do
	itext t 1 "http://code.google.com/p/msysgit/downloads/list", \t -> do
	text t "* Git-1.9.0-preview20140217.exeをダウンロード", \t -> do
	text t "* Adjusting your PATH ...では", \t -> do
	itext t 1 "- Run Git and include ...を選択すると良いかも", \t -> do
	text t "* Choosing the SSH ...ではUse OpenSSH ...を", \t -> do
	text t "* Configuring the line ...ではCheckout Windows ...で良いと思う"
	itext t 1 "git config --global core.autocrlf true"
	itext t 1 "git config --global -l", \t -> do
	text t "* git bashを試してみる", \t -> do
	itext t 1 "- vimが入っているかどうか", \t -> do
	itext t 1 "- git bashからnotepadが使えるか", \t -> do
	itext t 1 "- 等々"
 ]

tryGitBash :: Page
tryGitBash = [\t -> do
	writeTopTitle t "gitのチェック"
	text t "", \t -> do
	text t "* まずはgit bashを開く", \t -> do
	text t "* どこにいるか確認する", \t -> do
	itext t 1 "% pwd", \t -> do
	text t "* 講義用のリポジトリをcloneする", \t -> do
	itext t 0 "% git clone"
	itext t 0.5 "https://github.com/YoshikuniJujo/haskell_lecture.git"
	itext t 0.5 "lecture", \t -> do
	itext t 0 "% cd lecture/studentPC/lectures/", \t -> do
	itext t 0 "% ls"
 ]

installGHC :: Page
installGHC = [\t -> do
	writeTopTitle t "ghcのインストール"
	text t "", \t -> do
	text t "* 日本語名のアカウントだと問題が生じる可能性がある", \t -> do
	text t "* 英語名のアカウントを用意すること", \t -> do
	text t "* 以下のサイトから", \t -> do
	itext t 1 "http://www.haskell.org/platform/windows.html", \t -> do
	text t "* 以下をダウンロードし", \t -> do
	itext t 1 "Haskell Platform 2013.2.0.0 for Windows", \t -> do
	text t "* Setup.exeを実行する"
 ]

installMinGW :: Page
installMinGW = [\t -> do
	writeTopTitle t "MinGWのインストール"
	text t "", \t -> do
	text t "* MinGW 5.1.6をインストールする", \t -> do
	text t "* 以下のサイトから", \t -> do
	itext t (- 1.4) "http://sourceforge.net/projects/mingw/files/OldFiles/MinGW 5.1.6/", \t -> do
	text t "* MinGW-5.1.6.exeをダウンロードし実行する", \t -> do
	text t "* C++ compilerオプションを選択する必要がある"
 ]

installMSYS :: Page
installMSYS = [\t -> do
	writeTopTitle t "MSYSのインストール"
	text t "", \t -> do
	text t "* MSYS 1.0.11をインストールする", \t -> do
	text t "* 以下のファイルをダウンロードし", \t -> do
	itext t (- 1) "http://downloads.sourceforge.net/mingw/MSYS-1.0.11.exe", \t -> do
	text t "* 実行する"
 ]

installWxHaskell :: Page
installWxHaskell = [\t -> do
	writeTopTitle t "wxHaskellのインストール"
	text t "", \t -> do
	text t "* wxHaskellをインストールする", \t -> do
	text t "* 基本的には以下のサイトの指示に従う", \t -> do
	itext t (- 1) "http://www.haskell.org/haskellWiki/WxHaskell/Windows"
	text t "* パスを通す", \t -> do
	itext t 1 "set path=C:\\MinGW\\bin;%path%"
 ]

installWxPack :: Page
installWxPack = [\t -> do
	writeTopTitle t "wxPackのインストール"
	text t "", \t -> do
	text t "* wxWidgetsとwxHaskellのpre-build distributionを見つけた", \t -> do
	text t "* それがwxPack", \t -> do
	text t "* これを使えば簡単にインストールできる、と思う", \t -> do
	text t "* 以下のサイトからダウンロードし実行", \t -> do
	itext t 1 "http://wxpack.sourceforge.net"
 ]

installNano :: Page
installNano = [\t -> do
	writeTopTitle t "nanoエディタのインストール"
	text t "", \t -> do
	text t "* nanoはシンプルなエディタ", \t -> do
	text t "* 以下のページからnano-2.2.6.zipをダウンロードし展開", \t -> do
	itext t 1 "http://www.nano-editor.org/download.php", \t -> do
	text t "* bin, usrフォルダをユーザーフォルダにコピー", \t -> do
	text t "* PATHに%USERPROFILE%\\binを加える"
 ]

installEmacs :: Page
installEmacs = [\t -> do
	writeTopTitle t "Emacsエディタのインストール"
	text t "", \t -> do
	text t "* emacsは一部で非常に人気のあるエディタ", \t -> do
	text t "* 以下のページからemacs-24.3-bin-i386.zipをダウンロード", \t -> do
	itext t 1 "http://ftp.gnu.org/gnu/emacs/windows/", \t -> do
	text t "* 展開する", \t -> do
	text t "* フォルダをC:にコピーしemacsへrenameする", \t -> do
	text t "* ?PATHを通す?"
 ]

testSome :: Page
testSome = [\t -> do
	writeTopTitle t "テスト"
	text t "", \t -> do
	text t "* git bashからvim, nano, emacsを使ってみる", \t -> do
	text t "* ghciを試す", \t -> do
	text t "* 簡単なソースコードをコンパイル実行してみる", \t -> do
	text t "* wxHaskellのソースコードをコンパイル実行してみる"
 ]

others :: Page
others = [\t -> do
	writeTopTitle t "その他"
	text t "", \t -> do
	text t "* 各講義日が終わった段階で「自習課題」を渡すことを検討", \t -> do
	text t "* 紙のテストとする", \t -> do
	text t "* 次回の講義時に任意で提出してもらい理解度をチェックする", \t -> do
	text t "* 課題を用意する時間があれば、だが"
 ]

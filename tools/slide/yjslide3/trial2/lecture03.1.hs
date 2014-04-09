import Lecture

import Data.Time
import Control.Applicative
import System.IO.Unsafe

today :: Day
today = unsafePerformIO $ localDay . zonedTimeToLocalTime <$> getZonedTime

subtitle :: String
subtitle = "トライアル 第3.1回 HHKBに慣れよう"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, whats,
	keyboard, editor, nano, backspace, backspace2, backspaceSummary,
	symbols,
	equal, plus, equalPlus, minus, asterisk, slash, circumflex, tilde,
	semicolon, colon, quote, doubleQuote, backSlash, atMark, ampersand,
	verticalBar, parenthesis, underscore, bracket, brace, question,
	save, summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* この回の内容はキーボードの操作を学ぶ", \t -> do
--	text t "* 講義で使用するキーボード、エディタ、シェルの操作", \t -> do
	text t "* 手を動かすことで講義内容が身につく", \t -> do
	text t "* そのためには", \t -> do
	text t "* まずはレクチャー用の環境に慣れてもらうと効率的", \t -> do
	text t "* 今回を含めた導入の講義で以下の操作を学ぶ", \t -> do
	itext t 1 "キーボード、エディタ、シェル、ghcの対話環境"
 ]

whats :: Page
whats = [\t -> do
	writeTopTitle t "何を学ぶのか"
	text t "", \t -> do
	text t "* 以下の操作について学んでいく", \t -> do
	text t "* 講義で使うキーボードの使いかた", \t -> do
	text t "* nanoエディタの使いかた", \t -> do
	text t "* シェル(コマンドプロンプト)の使いかた", \t -> do
	text t "* Haskellの対話環境の使いかた"
	text t "", \t -> do
	itext t 0 "(注) この講義では以下の3つをほぼ同じ意味に用いる", \t -> do
	itext t 1 "シェル", \t -> do
	itext t 1 "ターミナル", \t -> do
	itext t 1 "コマンドプロンプト"
 ]

keyboard :: Page
keyboard = [\t -> do
	writeTopTitle t "キーボード"
	text t "", \t -> do
	text t "* 今回用意したキーボードはHappy Hacking Keyboard", \t -> do
	itext t 1 "- 以降はHHKBと表記することにする", \t -> do
	text t "* HHKBの英語配列キーボードを用意した", \t -> do
	text t "* 不必要なキーを極力排除しシンプルでコンパクト", \t -> do
	text t "* そのため、入力のしかたにすこし癖がある", \t -> do
	text t "* nanoエディタのなかでHHKBの操作を学ぶ", \t -> do
	text t "* キーの位置についてざっと紹介する", \t -> do
	text t "* もちろん、今全部覚える必要はない"
 ]

editor :: Page
editor = [\t -> do
	writeTopTitle t "エディタ"
	text t "", \t -> do
	text t "* 今回用意したエディタは以下の3つ", \t -> do
	itext t 1 "nano, vim, emacs", \t -> do
	text t "* この3つから選んでもらうことになる", \t -> do
	text t "* vimやemacsを選ぶ人に対しては", \t -> do
	itext t 1 "「いつものように編集してください」と言っておこう", \t -> do
	text t "* ちなみに僕はvim userなのでvimについてなら教えられる", \t -> do
	text t "* どれも使ったことがないという人はnanoを選ぼう"
 ]

nano :: Page
nano = [\t -> do
	writeTopTitle t "nano"
	text t "", \t -> do
	text t "* nanoのなかでHHKBの操作について学ぶ", \t -> do
	text t "* デスクトップに左右2つのターミナルがあると思う", \t -> do
	text t "* どちらかに以下のように打ち込もう", \t -> do
	itext t 1 "% nano -w"
	itext t 1 "(%はシェルのプロンプトなので打ち込まない)", \t -> do
	text t "* 入力例において%ではじめた場合はシェルからの入力を示す", \t -> do
	text t "* -wオプションはワードラップ処理を無効化する", \t -> do
	text t "* ワードラップ処理とは", \t -> do
	itext t 1 "入力行が長くなったときに自動で改行を挿入する処理"
 ]

backspace :: Page
backspace = [\t -> do
	writeTopTitle t "バックスペース"
	text t "", \t -> do
	text t "* HHKBにはバックスペースキーがない", \t -> do
	text t "* バックスペースを入力したいときには", \t -> do
	itext t 1 "Fn + Delete", \t -> do
	text t "* やってみよう", \t -> do
	text t "* エディタに適当な文字たとえば以下を入力する", \t -> do
	itext t 1 "miss", \t -> do
	text t "* ここでFn + Deleteを4回入力してみよう", \t -> do
	text t "* 文字列が消えたはずだ"
 ]

backspace2 :: Page
backspace2 = [\t -> do
	writeTopTitle t "バックスペース"
	text t "", \t -> do
	text t "* バックスペースの入力には実はもっとお勧めの方法がある", \t -> do
	text t "* 以下のキーストロークで入力可能だ", \t -> do
	itext t 1 "Control + h", \t -> do
	text t "* HHKBではControlキーが左手小指のすぐ左にある", \t -> do
	text t "* これだとほぼホームポジションのままで入力できる", \t -> do
	text t "* やってみよう", \t -> do
	text t "* エディタに適当な文字たとえば以下を入力する", \t -> do
	itext t 1 "machigaeta", \t -> do
	text t "* ここでControl + hを10回入力してみよう", \t -> do
	text t "* 文字列が消えたはずだ"
 ]

backspaceSummary :: Page
backspaceSummary = [\t -> do
	writeTopTitle t "バックスペース"
	text t "", \t -> do
	text t "* 入力の最中にバックスペースが使いたくなったら", \t -> do
	itext t 1 "- 以下のいずれかのキーストロークで入力する", \t -> do
	itext t 1 "1. Fn + Delete", \t -> do
	itext t 1 "2. Control + h", \t -> do
	text t "* 好きなほうを使おう"
 ]

symbols :: Page
symbols = [\t -> do
	writeTopTitle t "記号"
	text t "", \t -> do
	text t "* 今回用意したのは英語配列のキーボードなので", \t -> do
	itext t 1 "記号の位置が日本語配列とは異なる", \t -> do
	text t "* 以下の記号の位置について学んでいこう", \t -> do
	itext t 1 "= + - * / ^ ~ ; : ' \""
	itext t 1 "\\ @ & | ( ) _ [ ] { }"
 ]

equal :: Page
equal = [\t -> do
	writeTopTitle t "="
	text t "", \t -> do
	text t "* まずは=の位置を学ぼう", \t -> do
	text t "* 1234567890と打ってみよう", \t -> do
	text t "* さらに0を打った右手小指を右にずらしながら入力を続ける", \t -> do
	itext t 1 "1234567890右へ右へ右へ右へ", \t -> do
	text t "* もう一度くりかえす", \t -> do
	itext t 1 "1234567890右へ右へ右へ右へ", \t -> do
	text t "* 以下のように入力されたはずだ", \t -> do
	itext t 1 "1234567890-=\\`1234567890-=\\`", \t -> do
	text t "* =を入力するには以下のようにする", \t -> do
	itext t 1 "0の右の右のキーを押す"
 ]

plus :: Page
plus = [\t -> do
	writeTopTitle t "+"
	text t "", \t -> do
	text t "* +の入力を見てみよう", \t -> do
	text t "* Shiftキーを押しながら1234567890-=\\`を入力する", \t -> do
	itext t 1 "1234567890右へ右へ右へ右へ", \t -> do
	text t "* もう一度くりかえす", \t -> do
	itext t 1 "1234567890右へ右へ右へ右へ", \t -> do
	text t "* 以下のように入力されたはずだ", \t -> do
	itext t 1 "!@#$%^&*()_+|~!@#$%^&*()_+|~", \t -> do
	text t "* +を入力するときには以下のようにすれば良い", \t -> do
	itext t 1 "Shift + ="
 ]

equalPlus :: Page
equalPlus = [\t -> do
	writeTopTitle t "=と+の練習"
	text t "", \t -> do
	text t "* =と+の練習をしてみよう", \t -> do
	text t "* 以下のように打ち込もう", \t -> do
	itext t 1 "===+++===", \t -> do
	itext t 1 "=+=+=+=+=", \t -> do
	itext t 1 "x+=1", \t -> do
	itext t 1 "1+1=2", \t -> do
	itext t 1 "2+2=4", \t -> do
	itext t 1 "4+4=8", \t -> do
	itext t 1 "1+1+1=3"
 ]

minus :: Page
minus = [\t -> do
	writeTopTitle t "-"
	text t "", \t -> do
	text t "* -の位置を学ぶ", \t -> do
	text t "* 以下のように打ち込もう", \t -> do
	itext t 1 "1234567890-=\\`1234567890-=\\`", \t -> do
	text t "* -は0のひとつ右にあることがわかる", \t -> do
	text t "* 以下の練習をしよう", \t -> do
	itext t 1 "1-1+1=1", \t -> do
	itext t 1 "12-5=7", \t -> do
	itext t 1 "090-0110-1001", \t -> do
	itext t 1 "good-bye", \t -> do
	itext t 1 "-- single-line comment"
 ]

asterisk :: Page
asterisk = [\t -> do
	writeTopTitle t "*"
	text t "", \t -> do
	text t "* 乗算等に使われる*の位置について学ぶ", \t -> do
	text t "* Shiftキーを押しながら1234567890としてみよう", \t -> do
	text t "* 以下のようになるはずだ", \t -> do
	itext t 1 "!@#$%^&*()", \t -> do
	text t "* *を入力するためには以下のようにする", \t -> do
	itext t 1 "Shift + 8", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "8*8=64", \t -> do
	itext t 1 "8*11=88", \t -> do
	itext t 1 "8*8+24=88", \t -> do
	itext t 1 "3*7=21", \t -> do
	itext t 1 "/* C-style comment */"
 ]

slashToday :: String
slashToday = let (y, m, d) = toGregorian today in
	show y ++ "/" ++ show m ++ "/" ++ show d

slash :: Page
slash = [\t -> do
	writeTopTitle t "/"
	text t "", \t -> do
	text t "* 除算等に使われる/の位置について学ぶ", \t -> do
	text t "* 以下のように打ち込んでみよう", \t -> do
	itext t 1 "azsxdcfvgbhnjmk,l.;/", \t -> do
	text t "* 左から順に中段下段中段下段と打ち込んでいる", \t -> do
	text t "* /は右小指の下段にあることがわかる", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "1.5/0.5=3.0", \t -> do
	itext t 1 "SSL/TSL", \t -> do
	itext t 1 $ concat $ replicate 1 slashToday, \t -> do
	itext t 1 "// C-style single-line comment"
 ]

circumflex :: Page
circumflex = [\t -> do
	writeTopTitle t "^"
	text t "", \t -> do
	text t "* 文字^は「サーカムフレックス」と呼ぶ", \t -> do
	text t "* 「ハット」とも呼ぶ", \t -> do
	text t "* 「カレット」とも呼ぶが、本来は別の記号らしい", \t -> do
	text t "* Shiftキーを押しながら1234567890とする", \t -> do
	text t "* 以下のように入力されるはずだ", \t -> do
	itext t 1 "!@#$%^&*()", \t -> do
	text t "* ^(ハット)を入力するには以下のようにする", \t -> do
	itext t 1 "Shift + 6", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "6^6=46656", \t -> do
	itext t 1 "2^2^2=16"
 ]

tilde :: Page
tilde = [\t -> do
	writeTopTitle t "~"
	text t "", \t -> do
	text t "* ~の位置について学ぶ", \t -> do
	text t "* ~は「チルダ」と呼ぶ", \t -> do
	text t "* Shiftを押しながら1234567890右へ右へ右へ右へ", \t -> do
	text t "* 以下のように入力されるはずだ", \t -> do
	itext t 1 "!@#$%^&*()_+|~", \t -> do
	text t "* チルダを入力するには", \t -> do
	itext t 1 "Shift + 「=の右の右」", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "~/foo/bar"
 ]

semicolon :: Page
semicolon = [\t -> do
	writeTopTitle t ";"
	text t "", \t -> do
	text t "* ;の位置について学ぶ", \t -> do
	text t "* 以下のように打ってみよう", \t -> do
	itext t 1 "asdfghjkl;", \t -> do
	text t "* ;を入力するには", \t -> do
	itext t 1 "ホームポジションで右手小指", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "print a; print b; print c;"
 ]

colon :: Page
colon = [\t -> do
	writeTopTitle t ":"
	text t "", \t -> do
	text t "* :の位置について学ぶ", \t -> do
	text t "* 以下のように打ってみる", \t -> do
	itext t 1 "ASDFGHJKL:", \t -> do
	text t "* :を打つには", \t -> do
	itext t 1 "Shift + ホームポジションで左手小指", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "x :: Int; y :: Double; z :: Bool"
 ]

quote :: Page
quote = [\t -> do
	writeTopTitle t "'"
	text t "", \t -> do
	text t "* 'はアポストロフィと呼ぶ", \t -> do
	text t "* 本来は違うものだが一般的にはクォートとも呼ばれる", \t -> do
	text t "* シングルクォートとも呼ぶ", \t -> do
	text t "* 以下のように入力してみよう", \t -> do
	itext t 1 "asdfghjkl;'", \t -> do
	text t "* 'を入力するには以下のようにする", \t -> do
	itext t 1 ";のひとつ右", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "That's right.", \t -> do
	itext t 1 "'a'"
 ]

doubleQuote :: Page
doubleQuote = [\t -> do
	writeTopTitle t "\""
	text t "", \t -> do
	text t "* \"はダブルクォートと呼ばれる", \t -> do
	text t "* 以下のように入力してみよう", \t -> do
	itext t 1 "ASDFGHJKL:\"", \t -> do
	text t "* \"を入力するには", \t -> do
	itext t 1 "Shift + '", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "You say \"Good-bye.\"", \t -> do
	itext t 1 "I say \"Hello.\""
 ]

backSlash :: Page
backSlash = [\t -> do
	writeTopTitle t "\\"
	text t "", \t -> do
	text t "* \\はバックスラッシュと呼ばれる", \t -> do
	text t "* 以下のように入力してみよう", \t -> do
	itext t 1 "1234567890-=\\`", \t -> do
	text t "* \\は以下のようにして入力できる", \t -> do
	itext t 1 "=のひとつ右", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "'\\n' == '\\x0a'", \t -> do
	itext t 1 "\\x -> x ^ x"
 ]

atMark :: Page
atMark = [\t -> do
	writeTopTitle t "@"
	text t "", \t -> do
	text t "* @はアットマークと呼ばれる", \t -> do
	text t "* Shiftキーを押しながら1234567890と入力する", \t -> do
	itext t 1 "!@#$%^&*()", \t -> do
	text t "* @は以下のように入力する", \t -> do
	itext t 1 "Shift + 2", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "foo@gmail.com"
 ]

ampersand :: Page
ampersand = [\t -> do
	writeTopTitle t "&"
	text t "", \t -> do
	text t "* &はアンパサンドと読む", \t -> do
	text t "* Shiftキーを押しながら1234567890と入力する", \t -> do
	itext t 1 "!@#$%^&*()", \t -> do
	text t "* &は以下のように入力する", \t -> do
	itext t 1 "Shift + 7", \t -> do
	text t "* 日本語キーボードとはひとつずれる", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "True && False"
 ]

verticalBar :: Page
verticalBar = [\t -> do
	writeTopTitle t "|"
	text t "", \t -> do
	text t "* |はバーティカルバーと読む", \t -> do
	text t "* Shiftキーを押しながら1234567890右へ右へ右へ右へと入力", \t -> do
	itext t 1 "!@#$%^&*()_+|~", \t -> do
	text t "* |は以下のように入力する", \t -> do
	itext t 1 "Shift + \\(=のひとつ右)", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "True || False"
 ]

parenthesis :: Page
parenthesis = [\t -> do
	writeTopTitle t "()"
	text t "", \t -> do
	text t "* ()は丸括弧やパーレンと呼ばれる", \t -> do
	text t "* Shiftキーを押しながら1234567890と入力", \t -> do
	itext t 1 "!@#$%^&*()", \t -> do
	text t "* パーレンは以下のように入力する", \t -> do
	itext t 1 "Shift + 9", \t -> do
	itext t 1 "Shift + 0", \t -> do
	text t "* 日本語配列とはひとつずれる", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "(1 + 2) * (3 - 4)"
 ]

underscore :: Page
underscore = [\t -> do
	writeTopTitle t "_"
	text t "", \t -> do
	text t "* _はアンダースコアと呼ぶ", \t -> do
	text t "* アンダーバーとも呼ばれるが、これは和製英語", \t -> do
	text t "* Shiftキーを押しながら123456790右へ右へ右へ右へ", \t -> do
	itext t 1 "!@#$%^&*()_+|~", \t -> do
	text t "* _を入力するには以下のようにする", \t -> do
	itext t 1 "Shift + -", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "this_is_snake_case"
 ]

bracket :: Page
bracket = [\t -> do
	writeTopTitle t "[]"
	text t "", \t -> do
	text t "* []は角括弧またはブラケットと呼ぶ", \t -> do
	text t "* qwertyuiop右へ右へと入力してみよう", \t -> do
	text t "* 以下のように入力されるはずだ", \t -> do
	itext t 1 "qwertyuiop[]", \t -> do
	text t "* []は以下のように入力できる", \t -> do
	itext t 1 "pの右", \t -> do
	itext t 1 "さらにその右", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "['a', 'b']"
 ]

brace :: Page
brace = [\t -> do
	writeTopTitle t "{}"
	text t "", \t -> do
	text t "* {}は波括弧またはブレースと呼ぶ", \t -> do
	text t "* Shiftキーを押しながらqwertyuiop右へ右へ", \t -> do
	itext t 1 "QWERTYUIOP{}", \t -> do
	text t "* {}は以下のように入力できる", \t -> do
	itext t 1 "Shift + [", \t -> do
	itext t 1 "Shift + ]", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "greeting {txt = \"hello\"}"
 ]

question :: Page
question = [\t -> do
	writeTopTitle t "?"
	text t "", \t -> do
	text t "* Shiftキーを押しながらzxcvbnm右へ右へ右へ", \t -> do
	itext t 1 "ZXCVBNM<>?", \t -> do
	text t "* ?は以下のように入力する", \t -> do
	itext t 1 "Shift + /", \t -> do
	text t "* 以下の練習をしてみよう", \t -> do
	itext t 1 "Is this a pen?", \t -> do
	itext t 1 "No. This is a cabbage."
 ]

save :: Page
save = [\t -> do
	writeTopTitle t "保存終了"
	text t "", \t -> do
	text t "* 今までの練習の成果を一応保存しておこう", \t -> do
	text t "* 保存はCtrl + o", \t -> do
	text t "* ファイル名(例: hhkb_practice.txt)を入力してreturn", \t -> do
	text t "* 終了はCtrl + x"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* HHKB(英語配列)のキー配列について学んだ", \t -> do
	text t "* Shiftを押すことによる変化がある程度規則的", \t -> do
	itext t 1 "- -> _, ' -> \", ; -> :", \t -> do
	text t "* また、[と]がちゃんと並んでいる", \t -> do
	text t "* 日本語配列は無理にカナキーを押し込むことで", \t -> do
	itext t 1 "ゆがんだキー配列になってしまっている", \t -> do
	text t "* カナ入力をしなければ日本語配列を使うメリットはない"
 ]

import Data.Char

import Lecture

subtitle :: String
subtitle = "第17回 モナドの復習と演習"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutModule,
	cagedLion, cagedLion2, cagedLion3, cagedLion4, cagedLion5,
	cagedLion6, cagedLion7, cagedLion8, cagedLion9, cagedLionSummary,
	aboutLogger, aboutLogger2, aboutLogger3, aboutLogger4, aboutLogger5,
	aboutLogger6, aboutLogger7, aboutLogger8, aboutLogger9, aboutLogger10,
	aboutLogger11, aboutLogger12, aboutLogger13, aboutLogger14,
	aboutLogger15, aboutLogger16, aboutLogger17, aboutLogger18,
	aboutLogger19,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回はモナドについて学んだ", \t -> do
	text t "* すこし難しかったかもしれない", \t -> do
	text t "* 今回はもうすこし簡単な例を見る", \t -> do
	text t "* また演習問題を解くことで理解することを試みよう"
 ]

aboutModule :: Page
aboutModule = [\t -> do
	writeTopTitle t "モジュールシステム"
	text t "", \t -> do
	text t "* 今まではモジュールについてあまり意識してこなかった", \t ->
	text t "* しかし、すでに2つのモジュールに触れている", \t -> do
	itext t 1 "- Prelude: 基本的な関数が定義されているモジュール", \t -> do
	itext t 1 "- Main: モジュール宣言を省略した場合のデフォルト", \t -> do
	text t "* モジュール宣言は以下の形式となる", \t -> do
	itext t 1 "module [モジュール名] ([エクスポートリスト]) where", \t -> do
	text t "* エクスポートリストは名前を','で分けたリスト", \t -> do
	text t "* 値構築子のエクスポートは特別な形となる", \t -> do
	itext t 1 "[型名]([値構築子名])", \t -> do
	text t "* data Foo = BarのようなときのBarをエクスポートするには", \t -> do
	itext t 1 "- module Baz (Foo(Bar)) whereのようにする"
 ]

cagedLion :: Page
cagedLion = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* 檻に入れたライオンをエクスポートするモジュールを作る", \t -> do
	text t "* ライオンは状態Hungry, Normal, Fullを持つ", \t -> do
	text t "* play関数でライオンは空腹の方向に変化し", \t -> do
	text t "* feed関数でライオンは満腹の方向に変化する", \t -> do
	text t "* ライオンを操作するときのみ檻から出し", \t -> do
	itext t 1 "- 操作後は絶対に檻の外にいてはならない", \t -> do
	text t "* lectures/lecture17ディレクトリを作りLion.hsを作ろう"
 ]

data Lion = Lion Name State deriving Show
data State = Hungry | Normal | Full deriving Show
type Name = String

cagedLion2 :: Page
cagedLion2 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* ライオンの持つ状態を定義しよう", \t -> do
	itext t 1 "data State = Hungry | Normal | Full deriving Show", \t -> do
	text t "* ライオンは名前と状態を持つことにする", \t -> do
	itext t 1 "type Name = String", \t -> do
	itext t 1 "data Lion = Lion Name State deriving Show", \t -> do
	text t "* これらをLion.hsに書き込もう"
 ]

cagedLion3 :: Page
cagedLion3 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* 檻を定義してみよう", \t -> do
	itext t 1 "newtype Caged a = Caged a deriving Show", \t -> do
	text t "* はじめのCagedは型構築子で2つめのCagedは値構築子", \t -> do
	text t "* Caged aはa型の値をひとつ取る値構築子Cagedで作れる", \t -> do
	text t "* このCagedをMonadクラスのインスタンスにしてみる", \t -> do
	itext t 1 "instance Monad Caged where", \t -> do
	itext t 2 "return = Caged", \t -> do
	itext t 2 "Caged x >>= f = f x", \t -> do
	text t "* これらをLion.hsに書き込もう"
 ]

-- Mufasa, Sarabi, Simba, Sarafina, Nala, Scar

newtype Caged a = Caged a deriving Show

instance Monad Caged where
	return = Caged
	Caged x >>= f = f x

cagedLion4 :: Page
cagedLion4 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* ライオンを生み出す関数を作ろう", \t -> do
	text t "* ライオンは檻のなかに生まれることにする", \t -> do
	itext t 1 "lion :: Name -> Caged Lion", \t -> do
	itext t 1 "lion n = Caged $ Lion n Hungry", \t -> do
	text t "* ライオンに餌を与える関数", \t -> do
	itext t 1 "feed :: Lion -> Lion", \t -> do
	itext t 1 "feed (Lion n Hungry) = Lion n Normal", \t -> do
	itext t 1 "feed (Lion n _) = Lion n Full", \t -> do
	text t "* これらをLion.hsに書き込もう"
 ]

lion :: Name -> Caged Lion
lion n = Caged $ Lion n Hungry

feed :: Lion -> Lion
feed (Lion n Hungry) = Lion n Normal
feed (Lion n Normal) = Lion n Full
feed (Lion n _) = Lion n Full

cagedLion5 :: Page
cagedLion5 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* ライオンと遊ぶ関数", \t -> do
	itext t 1 "play :: Lion -> Lion", \t -> do
	itext t 1 "play (Lion n Full) = Lion n Normal", \t -> do
	itext t 1 "play (Lion n _) = Lion n Hungry", \t -> do
	text t "* これをLion.hsに書き込もう"
 ]

play :: Lion -> Lion
play (Lion n Full) = Lion n Normal
play (Lion n Normal) = Lion n Hungry
play (Lion n _) = Lion n Hungry

cagedLion6 :: Page
cagedLion6 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* コードの先頭にモジュール宣言をつけよう", \t -> do
	text t "* モジュール名はLionとする", \t -> do
	text t "* 使用する型はLionとCagedなのでそれはエクスポートする", \t -> do
	text t "* 値構築子のLionをエクスポートすると", \t -> do
	itext t 1 "- 檻の外でライオンを生み出すことができてしまう", \t -> do
	itext t 1 "- それは危険なので値構築子Lionはエクスポートしない", \t -> do
	text t "* 檻のなかでライオンを生み出すlionをエクスポートする", \t -> do
	text t "* ライオンを扱う関数feed, playもエクスポートする", \t -> do
	text t "* よってモジュール宣言は以下のようになる", \t -> do
	itext t 1 "module Lion (Lion, Caged, lion, feed, play) where", \t -> do
	text t "* これをLion.hsに書き込もう"
 ]

cagedLion7 :: Page
cagedLion7 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "% ghci Lion.hs", \t -> do
	itext t 1 "*Lion> Lion \"danger\" Hungry", \t -> do
	itext t 1 $ show $ Lion "danger" Hungry, \t -> do
	itext t 2 "- 危ない!ライオンが檻の外にいる", \t -> do
	itext t 2 "- 値構築子Lionはエクスポートしていないはず", \t -> do
	text t "* モジュール名の前にある'*'がポイント", \t -> do
	text t "* この'*'はそのモジュールのなかにいますよ、という意味", \t -> do
	text t "* つまりライオンの生産地(多分アフリカ)にいるので", \t -> do
	itext t 1 "- 檻の外にライオンがいてもおかしくない"
 ]

cagedLion8 :: Page
cagedLion8 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	text t "* 文明社会のなかでライオンと戯れたい", \t -> do
	text t "* Lionモジュールの外にいてLionモジュールをimportしたい", \t -> do
	text t "* ghciでは以下のようにする", \t -> do
	itext t 1 "*Lion> :m Lion", \t -> do
	itext t 1 "Prelude Lion> ", \t -> do
	text t "* PreludeとLionモジュールがエクスポートする関数の使える", \t -> do
	itext t 1 "どこでもない場所にいることになる"
 ]

simba :: Caged Lion
simba = lion "Simba"

cagedLion9 :: Page
cagedLion9 = [\t -> do
	writeTopTitle t "ライオンの檻"
	text t "", \t -> do
	itext t 0 "Prelude Lion> lion \"Simba\"", \t -> do
	itext t 0 $ show $ lion "Simba", \t -> do
	itext t 0 "Prelude Lion> let simba = it", \t -> do
	itext t 0 "Prelude Lion> feed simba", \t -> do
	itext t 0 "... Couldn't match ... `Lion' with ... `Caged Lion' ...", \t -> do
	itext t 0 "Prelude Lion> simba >>= feed", \t -> do
	itext t 0 "... Couldn't match type `Lion' with `Caged b0' ...", \t -> do
	itext t 0 "Prelude Lion> simba >>= return . feed", \t -> do
	itext t 0 $ show $ simba >>= return . feed, \t -> do
	text t "* 餌を与えた後はちゃんと檻にもどしてあげる必要がある"
 ]

cagedLionSummary :: Page
cagedLionSummary = [\t -> do
	writeTopTitle t "ライオンの檻(まとめ)"
	text t "", \t -> do
	text t "* 檻に入れたライオンを輸出するモジュールを作った", \t -> do
	text t "* モナド関数では", \t -> do
	itext t 1 "- 何かを檻に入れることはできる", \t -> do
	itext t 1 "- 檻から一時的に出すことはできる", \t -> do
	itext t 1 "- しかし、檻から出しっぱなしにすることはできない", \t -> do
	text t "* モジュールを使って内部構造を隠蔽することができる", \t -> do
	text t "* CagedやLionの値構築子を輸出していないので", \t -> do
	text t "* 内部構造の変更は安全"
 ]

aboutLogger :: Page
aboutLogger = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* モナドとは単なる形式である", \t -> do
	text t "* 以下の型の関数が存在しモナド則を満たせばすべてモナド", \t -> do
	itext t 1 "m a -> (a -> m b) -> m b", \t -> do
	itext t 1 "a -> m a", \t -> do
	text t "* 以下のように書いても同じこと", \t -> do
	itext t 1 "(a -> m b) -> (b -> m c) -> (a -> m c)", \t -> do
	itext t 1 "(a -> b) -> (a -> m b)", \t -> do
	text t "* モナドという性質を共有していてもその中身は様々", \t -> do
	text t "* 今回の演習では計算のログを取るモナドを組み立ててみよう"
 ]

aboutLogger2 :: Page
aboutLogger2 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 計算をしながら計算のログ(記録)を取っていくモナドを作る", \t -> do
	text t "* ログは文字列のリストとしよう", \t -> do
	text t "* 例えば以下の関数があり", \t -> do
	itext t 1 "toCode :: Char -> Logger Int", \t -> do
	text t "* toCode 'c'は", \t -> do
	itext t 1 "- ログとして[\"toCode 'c'\"]を持ち", \t -> do
	itext t 1 "- 計算の結果として99を持つ", \t -> do
	text t "* つまり、Logger型は文字列のリストと結果の値を持つ", \t -> do
	text t "* 演習17-1. Logger型を定義してみよう", \t -> do
	itext t 1 "(1分)"
 ]

aboutLogger3 :: Page
aboutLogger3 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* できただろうか?", \t -> do
	text t "* できなくても大丈夫", \t -> do
	text t "* 大切なのは「頭を悩ませた」ということ", \t -> do
	text t "* 自分で考えたあとの解説は記憶に残りやすいし", \t -> do
	text t "* 以下の項目について理解する助けになる", \t -> do
	itext t 1 "- 「どうしてそうなのか」", \t -> do
	itext t 1 "- 「どうしてそうじゃないのか」", \t -> do
	text t "* 「理解」とは段階的なもの", \t -> do
	itext t 1 "- 何度も塗ることですこしずつ濃くしていけば良い", \t -> do
	itext t 1 "- 演者自身もHaskellの理解を日々濃くしている"
 ]

aboutLogger4 :: Page
aboutLogger4 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* Logger型の定義は", \t -> do
	itext t 1 "data Logger a = Logger [String] a", \t -> do
	text t "* 解答は同じだっただろうか", \t -> do
	text t "* 答えはひとつではない", \t -> do
	text t "* 以下の解も正解とする", \t -> do
	itext t 1 "newtype Logger a = Logger ([String], a)", \t -> do
	itext t 1 "data Logger a = Logger ([String], a)", \t -> do
	itext t 1 "type Logger a = ([String], a)", \t -> do
	text t "* もちろん[String]とaの順番が逆でも正解"
 ]

aboutLogger5 :: Page
aboutLogger5 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 今後の流れのために他の答えを出した人も以下を使おう", \t -> do
	itext t 1 "data Logger a = Logger [String] a deriving Show", \t -> do
	text t "* これをlogger.hsに書き込もう", \t -> do
	text t "* ログを残しつつ文字コードを求める関数", \t -> do
	itext t 1 "toCode :: Char -> Logger Int", \t -> do
	text t "* 以下のような値を返すものをするとする", \t -> do
	itext t 1 "toCode 'c'", \t -> do
	arrowIText t 1 "Logger [\"toCode 'c'\"] 99", \t -> do
	text t "* 演習17-2. toCodeを定義しよう", \t -> do
	itext t 1 "(import Data.Char (ord)が必要)", \t -> do
	itext t 1 "(1分)"
 ]

aboutLogger6 :: Page
aboutLogger6 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 解答は以下のようになる", \t -> do
	itext t 1 "toCode :: Char -> Logger Int", \t -> do
	itext t 1 "toCode c = Logger (\"toCode \" ++ show c) (ord c)", \t -> do
	text t "* 以下とあわせてlogger.hsに書き込もう", \t -> do
	itext t 1 "import Data.Char (ord)", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "Prelude Lion> :load logger.hs", \t -> do
	itext t 1 "*Main> toCode 'c'", \t -> do
	itext t 1 $ show $ toCode 'c'
 ]

data Logger a = Logger [String] a deriving Show

toCode, toCode' :: Char -> Logger Int
toCode c = Logger ["toCode " ++ show c] (ord c)

toCode' c = tell ("toCode " ++ show c) >> return (ord c)

aboutLogger7 :: Page
aboutLogger7 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 次に以下の関数を作ろうと思うのだが", \t -> do
	itext t 1 "double :: Int -> Logger Int", \t -> do
	itext t 1 "double 3", \t -> do
	arrowIText t 1 "Logger [\"double 3\"] 6", \t -> do
	text t "* その前に文字列をログにする関数を書こう", \t -> do
	text t "* 以下のような型になる", \t -> do
	itext t 1 "tell :: String -> Logger ()", \t -> do
	text t "* ログのほうだけを扱う関数なので", \t -> do
	itext t 1 "Logger aのaの部分を()で埋めている", \t -> do
	text t "* 演習17-3. tellを定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

tell :: String -> Logger ()
tell l = Logger [l] ()

aboutLogger8 :: Page
aboutLogger8 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "tell :: String -> Logger ()", \t -> do
	itext t 1 "tell l = Logger [l] ()", \t -> do
	text t "* これをlogger.hsに書き込もう", \t -> do
	text t "* これを使ってtoCodeを再定義したい", \t -> do
	text t "* さっきのtoCodeの定義を以下のように書き換えよう", \t -> do
	itext t 1 "toCode c ="
	itext t 2 "tell (\"toCode \" ++ show c) >> return (ord c)", \t -> do
	text t "* toCodeを以下の2つの部分に分けて構築している", \t -> do
	itext t 1 "- ログを追加", \t -> do
	itext t 1 "- 文字コードを返す"
 ]

aboutLogger9 :: Page
aboutLogger9 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 新しいtoCodeの定義はまだ使えない", \t -> do
	text t "* LoggerをMonadクラスのインスタンスにする必要がある", \t -> do
	text t "* 以下の関数を定義する必要がある", \t -> do
	itext t 1 "return :: a -> Logger a", \t -> do
	itext t 1 "(>>=) :: Logger a -> (a -> Logger b) -> Logger b", \t -> do
	text t "* まずは簡単なほうから", \t -> do
	text t "* returnは「何もせずに」値を包み込む関数", \t -> do
	text t "* Loggerについて言えば「ログを変化させずに」ということ", \t -> do
	itext t 1 "- ログを空にしておけば良い", \t -> do
	text t "* 演習17-4. Loggerのreturnを定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

instance Monad Logger where
	return = Logger []
	Logger l x >>= f = let Logger l' x' = f x in Logger (l ++ l') x'

aboutLogger10 :: Page
aboutLogger10 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "return x = Logger [] x", \t -> do
	text t "* これは以下のようにできる", \t -> do
	itext t 1 "return = Logger []", \t -> do
	text t "* これはMonadクラスのクラス関数なので", \t -> do
	itext t 1 "instance Monad Logger where", \t -> do
	itext t 2 "return = Logger []", \t -> do
	text t "* これをlogger.hsに書き込もう"
 ]

aboutLogger11 :: Page
aboutLogger11 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "((>>=)が定義されていないというWarningが出る)", \t -> do
	itext t 1 "*Main> return 8 :: Logger Int", \t -> do
	itext t 1 $ show $ (return 8 :: Logger Int), \t -> do
	text t "* 問題なく定義できているようだ"
 ]

aboutLogger12 :: Page
aboutLogger12 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* それでは(>>=)の定義に移ろう", \t -> do
	itext t 1 "(>>=) :: Logger a -> (a -> Logger b) -> Logger b", \t -> do
	text t "* この関数に何をして欲しいか考える", \t -> do
	itext t 1 "- 第一引数のa型の値を第二引数である関数にわたして", \t -> do
	itext t 1 "- 出てきた結果について", \t -> do
	itext t 2 "ログのほうは第一引数のログに追加し", \t -> do
	itext t 2 "b型の値のほうは結果の値とする", \t -> do
	text t "* 演習17-5. Loggerの(>>=)を定義せよ", \t -> do
	itext t 1 "(2分)"
 ]


aboutLogger13 :: Page
aboutLogger13 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 難しかったかもしれない", \t -> do
	text t "* 順を追って見ていこう", \t -> do
	text t "* まずは引数のパターンマッチの部分を作ろう", \t -> do
	itext t 1 "(>>=) :: Logger a -> (a -> Logger b) -> Logger b", \t -> do
	text t "* 第一引数のLogger aは中身のログと値を使う", \t -> do
	text t "* 第二引数は関数なので分解できない", \t -> do
	itext t 1 "Logger l x >>= f = ...", \t -> do
	text t "* 第一引数の「値」にfを適用した結果が必要なので", \t -> do
	itext t 1 "Logger l x >>= f = ... f x ..."
 ]

aboutLogger14 :: Page
aboutLogger14 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 型とできた部分までを再掲する", \t -> do
	itext t 1 "(>>=) :: Logger a -> (a -> Logger b) -> Logger b", \t -> do
	itext t 1 "Logger l x >>= f = ... f x ...", \t -> do
	text t "* f xの結果のログと値を別々に使うのでパターンマッチする", \t -> do
	itext t 1 "Logger l x >>= f = let Logger l' x' = f x in ...", \t -> do
	text t "* 新しいログは古いログに追加し新しい値はそのまま返すので", \t -> do
	itext t 1 "Logger l x >>= f ="
	itext t 1.8 "let Logger l' x' = f x in Logger (l ++ l') x'"
 ]

aboutLogger15 :: Page
aboutLogger15 = [\t -> do
	writeTopTitle t "計算のログ", \t -> do
	text t "* logger.hsのインスタンス宣言に追加しよう", \t -> do
	itext t 0 "instance Monad Logger where", \t -> do
	itext t 1 "return = Logger []", \t -> do
	itext t 1 "Logger l x >>= f ="
	itext t 1.8 "let Logger l' x' = f x in Logger (l ++ l') x'", \t -> do
	text t "* これで前に再定義したtoCodeが動くようになる", \t -> do
	itext t 1 "toCode c ="
	itext t 2 "tell (\"toCode \" ++ show c) >> return (ord c)", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> toCode 'c'", \t -> do
	itext t 1 $ show $ toCode' 'c'
 ]

double :: Int -> Logger Int
double n = tell ("double " ++ show n) >> return (n * 2)

aboutLogger16 :: Page
aboutLogger16 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 次に以下のように整数を2倍するdoubleを考える", \t -> do
	itext t 1 "double :: Int -> Logger Int", \t -> do
	itext t 1 "double 8", \t -> do
	arrowIText t 1 $ show $ double 8, \t -> do
	text t "* これもtoCodeと同じように定義できる", \t -> do
	text t "* 演習17-6. doubleを定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

aboutLogger17 :: Page
aboutLogger17 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 以下のようになる。logger.hsに書き込もう", \t -> do
	itext t 1 "double :: Int -> Logger Int"
	itext t 1 "double n ="
	itext t 2 "tell (\"double \" ++ show n) >> return (n * 2)", \t -> do
	text t "* toCodeとdoubleを使えば", \t -> do
	itext t 1 "- ログを記録しながら", \t -> do
	itext t 1 "- 文字コードを2倍する関数toCodeDoubleが作れる", \t -> do
	text t "* 演習17-7. toCodeDoubleを定義せよ", \t -> do
	itext t 1 "(1分)"
 ]

toCodeDouble :: Char -> Logger Int
toCodeDouble c = toCode c >>= double

aboutLogger18 :: Page
aboutLogger18 = [\t -> do
	writeTopTitle t "計算のログ"
	text t "", \t -> do
	text t "* 以下のようになる", \t -> do
	itext t 1 "toCodeDouble :: Char -> Logger Int", \t -> do
	itext t 1 "toCodeDouble c = toCode c >>= double", \t -> do
	text t "* logger.hsに書き込み、試してみる", \t -> do
	itext t 1 "*Main> :reload", \t -> do
	itext t 1 "*Main> toCodeDouble 'c'", \t -> do
	itext t 1 $ show $ toCodeDouble 'c'
 ]

aboutLogger19 :: Page
aboutLogger19 = [\t -> do
	writeTopTitle t "計算のログ(まとめ)"
	text t "", \t -> do
	text t "* モナドとは型mについて", \t -> do
	itext t 1 "- (a -> m b)型の関数を次々につなげられるという性質", \t -> do
	text t "* 中身は何であれその性質を満たせばモナドである", \t -> do
	text t "* 結果とその過程を保存するモナドLoggerを作った", \t -> do
	itext t 1 "data Logger a = Logger [String] a", \t -> do
	text t "* Monadクラスへのインスタンス宣言は以下のようになる", \t -> do
	itext t 1 "instance Monad Logger where", \t -> do
	itext t 2 "return = Logger []", \t -> do
	itext t 2 "Logger l x >>= f = let (l', x') = f x in"
	itext t 3 "Logger (l ++ l') x'", \t -> do
	text t "* 関数適用の裏でログを結合している"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* ライオンをいれておく檻であるCaged型を作った", \t -> do
	text t "* Lion型やCaged型の値構築子を隠蔽することで", \t -> do
	itext t 1 "- 檻の外でライオンが作れないようにし", \t -> do
	itext t 1 "- 檻からライオンを出しっぱなしにできないようにした", \t -> do
	text t "* モナド関数のみを使うことで", \t -> do
	itext t 1 "- 檻から出したライオンを檻にもどすことを強制できる", \t -> do
	text t "* 演習では計算のログを保存するモナドを作ってみた", \t -> do
	text t "* このモナドでは表の計算と裏でのログの結合とが行われる"
 ]

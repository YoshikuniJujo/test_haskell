import Lecture

subtitle :: String
subtitle = "第17回 モナドの復習と演習"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	aboutModule,
	cagedLion, cagedLion2, cagedLion3, cagedLion4, cagedLion5,
	cagedLion6, cagedLion7, cagedLion8, cagedLion9, cagedLion10
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
	itext t 1 "feed (Lion n Normal) = Lion n Full", \t -> do
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
	itext t 1 "play (Lion n Normal) = Lion n Hungry", \t -> do
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
	text t "* 文明社会のなかでライオンと戯むれたい", \t -> do
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

cagedLion10 :: Page
cagedLion10 = [\t -> do
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

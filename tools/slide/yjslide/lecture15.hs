module Main where

import Lecture

subtitle :: String
subtitle = "第15回 いろいろなモナド"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude, monadList0,
	monadList1 1, maybeMonad, maybeMonad2,
	monadList1 2, errorMonad, errorMonad2, errorMonad3, errorMonad4,
	monadList1 3, listMonad, listMonad2, listMonad3,
	monadList1 4, stateMonad, stateMonad2, stateMonad3, stateMonad4,
		stateMonad5, stateMonad6, stateMonad7,
	monadList1 5, readerMonad, readerMonad2, readerMonad3,
		readerMonad4,
	monadList1 6, writerMonad, writerMonad2, writerMonad3,
		writerMonad4,
	{-
	monadList1 7, contMonad, contMonad2, contMonad3, contMonad4, contMonad5,
		contMonad6,
	-}
	monadList1 7
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* モナドは抽象的な概念", \t -> do
	text t "* それ自体としては理解しにくい", \t -> do
	text t "* これもモナドあれもモナドといろいろなモナドを見る", \t -> do
	text t "* その結果共通する構造が見えてくる", \t -> do
	text t "* 今回はいろいろなモナドについて見ていくことにする"
 ]

monadList :: [String]
monadList = [
	"Maybe", "Error", "List", "State", "Reader", "Writer", "IO"]

showMonadList :: Int -> Int -> String -> Turtle -> IO ()
showMonadList n i f = \t ->
	(if n == i then withRed t else id) $ text t $ show i ++ ". " ++ f

monadList0 :: Page
monadList0 = [\t -> writeTopTitle t "今回扱うモナド" >> text t ""] ++
	(zipWith (showMonadList 0) [1 ..] monadList)

monadList1 :: Int -> Page
monadList1 n = [\t -> oneshot t $ do
	writeTopTitle t "今回扱うモナド"
	oneshot t $ do
		text t ""
		mapM_ ($ t) $ (zipWith (showMonadList n) [1 ..] monadList)]

identityMonad :: Page
identityMonad = [\t -> do
	writeTopTitle t "Identityモナド"
	text t "", \t -> do
	text t "* 最も単純なモナド", \t -> do
	text t "* 単なる値", \t -> do
	text t "* 存在意義は?", \t -> do
	itext t 1 "- モナドの理解のため", \t -> do
	itext t 1 "- 後で扱うモナド変換子における役割"
 ]

identityMonad2 :: Page
identityMonad2 = [\t -> do
	writeTopTitle t "Identityモナド"
	text t "", \t -> do
	text t "* 型の定義は以下のようになる", \t -> do
	itext t 1 "type Identity a = a", \t -> do
	text t "* returnと(>>=)は以下のように定義される", \t -> do
	itext t 1 "return :: a -> Identity a"
	itext t 1 "return x = x", \t -> do
	itext t 1 "(>>=) :: Identity a -> (a -> Identity b)"
	itext t 6 "-> Identity b"
	itext t 1 "m >>= f = f m", \t -> do
	text t "* m >>= fは単なる関数適用となっている"
 ]

identityMonad3 :: Page
identityMonad3 = [\t -> do
	writeTopTitle t "Identityモナド"
	text t "", \t -> do
	text t "* Monadクラスのインスタンスにする", \t -> do
	itext t 1 "- 別名ではなくちゃんとした型にする必要がある"
	text t "", \t -> do
	text t "newtype Identity a = Identity { runIdentity :: a }", \t -> do
	text t "instance Monad Identity where"
	itext t 1 "return x = Identity x"
	itext t 1 "(Identity x) >>= f = f x"
 ]

identityMonad4 :: Page
identityMonad4 = [\t -> do
	writeTopTitle t "Identityモナド"
	text t "", \t -> do
	text t "使用例", \t -> do
	itext t 1 "runIdentity $ do"
	itext t 2 "x <- return 8"
	itext t 2 "y <- return 9"
	itext t 2 "return $ x + y", \t -> do
	text t "* 意味のある使用例を挙げにくい"
 ]

maybeMonad :: Page
maybeMonad = [\t -> do
	writeTopTitle t "Maybeモナド"
	text t "", \t -> do
	text t "* 基本的には前回の復習となる", \t -> do
	text t "* 失敗する可能性のある計算の連鎖を表現できる", \t -> do
	text t "* 型は以下のようになる", \t -> do
	itext t 1 "data Maybe a = Just a | Nothing", \t -> do
	text t "* インスタンス宣言は以下のようになる", \t -> do
	itext t 1 "instance Monad Maybe where"
	itext t 2 "return = Just"
	itext t 2 "Just x >>= f = f x"
	itext t 2 "Nothing >>= _ = Nothing"
 ]

maybeMonad2 :: Page
maybeMonad2 = [\t -> do
	writeTopTitle t "Maybeモナド"
	text t "", \t -> do
	text t "使用例:"
	itext t 1 "lookup2 :: a -> [(a, b)] -> [(b, c)] -> Maybe c"
	itext t 1 "lookup2 x dic1 dic2 = do"
	itext t 2 "y <- lookup x dic1"
	itext t 2 "lookup y dic2"
 ]

errorMonad :: Page
errorMonad = [\t -> do
	writeTopTitle t "Errorモナド"
	text t "", \t -> do
	text t "* Maybeモナドでは失敗はNothingで表現される", \t -> do
	text t "* 失敗の理由がひとつではない場合", \t -> do
	itext t 1 "- 失敗の理由が知りたくなるかもしれない", \t -> do
	itext t 1 "- 人間が読むなら失敗の理由は文字列で良いだろう", \t -> do
	text t "* Either a bは型aまたは型bの値を格納できるdata型", \t -> do
	itext t 1 "data Either a b = Left a | Right b", \t -> do
	text t "* Either String aをErrorモナドとして見ていこう"
 ]

errorMonad2 :: Page
errorMonad2 = [\t -> do
	writeTopTitle t "Errorモナド"
	text t "", \t -> do
	text t "instance Monad (Either String) where"
	itext t 1 "return = Right"
	itext t 1 "Right x >>= f = f x"
	itext t 1 "Left msg >>= _ = Left msg"
	text t "", \t -> do
	text t "* エラーを投げる関数もあると便利", \t -> do
	itext t 1 "throwError :: String -> Either String a"
	itext t 1 "throwError = Left"
 ]

errorMonad3 :: Page
errorMonad3 = [\t -> do
	writeTopTitle t "Errorモナド"
	text t "使用例:", \t -> do
	text t "eitherLookup ::"
	itext t 1 "Eq a => a -> [(a, b)] -> Either String b"
	text t "eitherLookup x dic = case lookup x dic of"
	itext t 1 "Just y -> Right y"
	itext t 1 "_ -> throwError \"no dictionary entry\""
	text t "", \t -> do
	text t "mod100 :: Int -> Either String Int"
	text t "mod100 0 = throwError \"division by 0\""
	text t "mod100 n = 100 `mod` n"
 ]

errorMonad4 :: Page
errorMonad4 = [\t -> do
	writeTopTitle t "Errorモナド"
	text t "使用例:", \t -> do
	text t "lookupMod100 :: a -> [(a, Int)] -> Either String Int"
	text t "lookupMod100 x dict = do"
	itext t 1 "n <- lookup x dict"
	itext t 1 "mod100 n"
	text t "", \t -> do
	text t "* エラーの理由がわかる", \t -> do
	itext t 1 "- 辞書にエントリーが無いとき"
	itext t 2 "Left \"no dictionary entry\"", \t -> do
	itext t 1 "- 辞書から取り出した値が0"
	itext t 2 "Left \"division by 0\""
 ]

listMonad :: Page
listMonad = [\t -> do
	writeTopTitle t "Listモナド"
	text t "", \t -> do
	text t "* 失敗する可能性のある関数は", \t -> do
	itext t 1 "- 見方を変えれば返す値が0または1個の関数", \t -> do
	text t "* 2個以上の値を返す関数を考えることができる", \t -> do
	itext t 1 "- [r1, r2, r3 ...]と表現できる", \t -> do
	itext t 1 "- Nothingは[], Just r1は[r1]に対応する", \t -> do
	text t "* リストをエラー系のモナドとして考えると", \t -> do
	itext t 1 "- 複数の値に次の関数を適用し", \t -> do
	itext t 1 "- それぞれに対し複数の値を返す"
 ]

listMonad2 :: Page
listMonad2 = [\t -> do
	writeTopTitle t "Listモナド"
	text t "", \t -> do
	text t "インスタンス宣言は以下のようになる", \t -> do
	itext t 1 "instance Monad [] where"
	itext t 2 "return x = [x]"
	itext t 2 "m >>= f = concat $ map f m"
	text t "", \t -> do
	text t "* 値を包み込むとはその値ひとつから成るリストを作ること", \t -> do
	text t "* mに含まれる複数の値のそれぞれに対してfを適用する", \t -> do
	text t "* fはリストを返すのでmap f mはリストのリストになる", \t -> do
	text t "* それぞれの値がそれぞれ候補となるので", \t -> do
	itext t 1 "- concatでリストを平坦にする"
 ]

listMonad3 :: Page
listMonad3 = [\t -> do
	writeTopTitle t "Listモナド"
	text t "", \t -> do
	text t "使用例:", \t -> do
	text t "* 将棋の桂馬が3回動いたときにどこにいるかを調べる", \t -> do
	itext t 1 "kmove :: (Int, Int) -> [(Int, Int)]"
	itext t 1 "kmove (x, y) = [(x - 1, y + 2), (x + 1, y + 2)]"
	itext t 1 "", \t -> do
	itext t 1 "keima3 :: [(Int, Int)]"
	itext t 1 "keima3 = do"
	itext t 2 "k1 <- kmove (2, 1)"
	itext t 2 "k2 <- kmove k1"
	itext t 2 "kmove k2"
 ]

stateMonad :: Page
stateMonad = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* 今まで見てきたのは失敗の可能性のある計算", \t -> do
	itext t 1 "- Listモナドはその自然な拡張", \t -> do
	text t "* Stateモナドはすこし性質が違う", \t -> do
	text t "* 状態を持つ計算を表現するモナド", \t -> do
	text t "* >>=は目に見えるところでは返り値をわたしている", \t -> do
	itext t 1 "- 裏では状態をわたしている"
 ]

stateMonad2 :: Page
stateMonad2 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* モナドを使わないで状態を扱う場合", \t -> do
	itext t 1 "- s -> (a, s)という型を持つ関数を使う", \t -> do
	itext t 1 "- 状態を取り、返り値と状態を返す", \t -> do
	itext t 1 "- 返された状態は次の関数にわたされる", \t -> do
	text t "* ランダムの例", \t -> do
	itext t 1 "random :: StdGen -> (Int, StdGen)", \t -> do
	itext t 1 "twoRandoms :: StdGen -> ((Int, Int), StdGen)"
	itext t 1 "twoRandoms g = let"
	itext t 2 "(n1, g1) = random g"
	itext t 2 "(n2, g2) = random g1 in"
	itext t 2 "((n1, n2), g2)"
 ]

stateMonad3 :: Page
stateMonad3 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* 以下の構造がある", \t -> do
	itext t 1 "someFun s0 = let"
	itext t 2 "(x1, s1) = fun1 s"
	itext t 2 "(x2, s2) = fun2 s1"
	itext t 2 "(x3, s3) = fun3 s2"
	itext t 2 "..."
	itext t 2 "in f x1 x2 x3 ...", \t -> do
	text t "* このような構造を抽出したのがStateモナドとなる", \t -> do
	itext t 1 "- 上の構造では最後の関数がそれまでの値を参照", \t -> do
	itext t 1 "- Stateモナドではもっと自由度が高い", \t -> do
	itext t 1 "- が、本質的には上の構造と同じ"
 ]

stateMonad4 :: Page
stateMonad4 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* まずはMonadクラスのインスタンスではないバージョン", \t -> do
	itext t 1 "- return, >>=は本当は別の名前にするべきだが", \t -> do
	itext t 1 "- とりあえずは、そのままにする", \t -> do
	text t "type State s a = s -> (a, s)"
	text t "return x = \\s -> (x, s)"
	text t "m >>= f = \\s -> let (v, s') = m s in f v s'"
	text t "", \t -> do
	text t "* return xは状態を変えずにxを返す", \t -> do
	text t "* m >>= fはまずはmを状態に適用して得た値にfを適用する", \t -> do
	itext t 1 "- (f v)がs -> (a, s)であることに注意", \t -> do
	itext t 1 "- (f v)に新しい状態s'を与えている"
 ]

stateMonad5 :: Page
stateMonad5 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* Monadクラスのインスタンスにする", \t -> do
	itext t 1 "- そのためにnewtypeとする必要がある"
	text t "", \t -> do
	text t "newtype State s a = State { runState :: s -> (a, s) }", \t -> do
	text t "instance Monad (State s) where"
	itext t 1 "return x = State $ \\s -> (a, s)"
	itext t 1 "State m >>= f = State $ \\s -> let"
	itext t 2 "(v, s') = m s in"
	itext t 2 "runState (f v) s'"
	text t "", \t -> do
	text t "* StateやrunStateはnewtypeの衣の着脱をしてるだけ"
 ]

stateMonad6 :: Page
stateMonad6 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "* 状態の更新と入手のための関数を作る", \t -> do
	itext t 1 "put :: s -> State s ()"
	itext t 1 "put s = State $ \\_ -> ((), s)", \t -> do
	itext t 1 "get :: State s s"
	itext t 1 "get = State $ \\s -> (s, s)"
 ]

stateMonad7 :: Page
stateMonad7 = [\t -> do
	writeTopTitle t "Stateモナド"
	text t "", \t -> do
	text t "getRandom :: State StdGen Int"
	text t "getRandom = do"
	itext t 1 "g <- get"
	itext t 1 "let (x, g') = random g"
	itext t 1 "put g'"
	itext t 1 "return x", \t -> do
	text t "fun :: State StdGen Int"
	text t "fun = do"
	itext t 1 "x <- getRandom"
	itext t 1 "y <- getRandom"
	itext t 1 "return $ x * y"
 ]

readerMonad :: Page
readerMonad = [\t -> do
	writeTopTitle t "Readerモナド"
	text t "", \t -> do
	text t "* ReaderモナドはStateモナドと似ている", \t -> do
	text t "* 違いはReaderモナドでは状態を変化させられない点", \t -> do
	text t "* それぞれの計算が初期状態を共有しているという構造", \t -> do
	text t "* 初期状態は「環境」と考えることもできる"
 ]

readerMonad2 :: Page
readerMonad2 = [\t -> do
	writeTopTitle t "Readerモナド"
	text t "", \t -> do
	text t "newtype Reader e a = Reader { runReader :: (e -> a) }"
	text t "", \t -> do
	text t "instance Monad (Reader e) where"
	itext t 1 "return x = Reader $ \\_ -> x"
	itext t 1 "Reader r >>= f = Reader $ \\e -> (f (r e)) e"
	text t "", \t -> do
	text t "* return xは環境に関係なくxを返す", \t -> do
	text t "* Reader r >>= fは環境を取って", \t -> do
	itext t 1 "- その環境にrを適用した結果の値にfを適用する", \t -> do
	itext t 1 "- その結果返ってきた(f (r e))自体もe -> aなので", \t -> do
	itext t 1 "- それにeを与える"
 ]

readerMonad3 :: Page
readerMonad3 = [\t -> do
	writeTopTitle t "Readerモナド"
	text t "", \t -> do
	text t "* 環境を問い合わせる関数も定義しておく", \t -> do
	itext t 1 "ask :: Reader e e"
	itext t 1 "ask = Reader $ \\e -> e"
 ]

readerMonad4 :: Page
readerMonad4 = [\t -> do
	writeTopTitle t "Readerモナド"
	text t "", \t -> do
	text t "使用例:", \t -> do
	text t "getVal :: String -> Reader [(String, Int)] Int"
	text t "getVal var = do"
	itext t 1 "env <- ask"
	itext t 1 "return $ fromMaybe 0 $ lookup var env",\t -> do
	text t "addVals ::"
	itext t 1 "String -> String -> Reader [(String, Int)] Int"
	text t "addVals varX varY = do"
	itext t 1 "x <- getVal varX"
	itext t 1 "y <- getVal varY"
	itext t 1 "return $ x + y"
 ]

writerMonad :: Page
writerMonad = [\t -> do
	writeTopTitle t "Writerモナド"
	text t "", \t -> do
	text t "* Stateモナドが入出力両方の機能だとして", \t -> do
	itext t 1 "- Readerモナドが入力機能を提供しているならば", \t -> do
	itext t 1 "- Writerモナドは出力機能を提供していると言える", \t -> do
	text t "* Stateモナドでは出力は次の入力に使われた", \t -> do
	text t "* Writerモナドには入力はない", \t -> do
	text t "* 出力は「使われる」代わりに「蓄えられる」", \t -> do
	text t "* 典型的な使い道はログを取ること", \t -> do
	text t "* 今回はログとしてStringを使うことにする"
 ]

writerMonad2 :: Page
writerMonad2 = [\t -> do
	writeTopTitle t "Writerモナド"
	text t "", \t -> do
	text t "newtype Writer a = Writer { runWriter :: (a, String) }", \t -> do
	text t "instance Monad Writer where"
	itext t 1 "return x = Writer (x, \"\")"
	itext t 1 "Writer (x, log) >>= f = let"
	itext t 2 "(x', log') = runWriter $ f x in"
	itext t 2 "Writer (x', log ++ log')"
	text t "", \t -> do
	text t "* return xはその値とログとして空文字列を返す", \t -> do
	text t "* Writer (x, log) >>= fはxにfを適用する", \t -> do
	itext t 1 "- その結果出てきた値を返り値とし", \t -> do
	itext t 1 "- 新たなログを前のログに追加する"
 ]

writerMonad3 :: Page
writerMonad3 = [\t -> do
	writeTopTitle t "Writerモナド"
	text t "", \t -> do
	text t "使用例:", \t -> do
	text t "two :: Writer Int"
	text t "two = do"
	itext t 1 "tell \"This is number 2.\\n\""
	itext t 1 "return 2", \t -> do
	text t ""
	text t "add :: Int -> Int -> Writer Int"
	text t "add x y = do"
	itext t 1 "tell \"Addition done.\\n\""
	itext t 1 "return $ x + y"
 ]

writerMonad4 :: Page
writerMonad4 = [\t -> do
	writeTopTitle t "Writerモナド"
	text t "", \t -> do
	text t "使用例:", \t -> do
	text t "twoPlusTwo :: Writer Int"
	text t "twoPlusTwo = do"
	itext t 1 "x <- two"
	itext t 1 "y <- two"
	itext t 1 "add x y"
 ]

contMonad :: Page
contMonad = [\t -> do
	writeTopTitle t "Continuationモナド"
	text t "", \t -> do
	text t "* 継続渡しスタイル(CPS)のモナド", \t -> do
	text t "* CPSとは何か?", \t -> do
	text t "* f(g(h(x)))を考えてみよう", \t -> do
	itext t 1 "- xにhを適用し、それをgに、さらにfにわたす", \t -> do
	text t "* 「それをgに、さらにfにわたす」がh(x)の時点での継続", \t -> do
	text t "* 継続渡しは関数の適用時に裏で行われている", \t -> do
	text t "* その値が次に適用される関数を継続と呼ぶ", \t -> do
	text t "* どこにでもあるものに名前をつける", \t -> do
	arrowIText t 1 "制御することができるようになる"
 ]

contMonad2 :: Page
contMonad2 = [\t -> do
	writeTopTitle t "Continuationモナド"
	text t "", \t -> do
	text t "* Haskellで明示的に継続を扱ってみよう", \t -> do
	text t "* 関数hから継続渡しスタイルの関数h'を作る", \t -> do
	itext t 1 "h' x = \\k -> k (h x)", \t -> do
	text t "* h' x gはg(h x)に簡約される", \t -> do
	text t "* つまりh'は第2引数に「継続」を取る", \t -> do
	text t "* 関数gも継続渡しスタイルにすると", \t -> do
	itext t 1 "h' x (\\y -> g' y f)", \t -> do
	text t "* 関数fも継続渡しスタイルにすると", \t -> do
	itext t 1 "h' x (\\y -> g' y (\\z -> f' z id))"
 ]

contMonad3 :: Page
contMonad3 = [\t -> do
	writeTopTitle t "Continuationモナド"
	text t "", \t -> do
	text t "* h', g', f'をつなぐ関数をつくる", \t -> do
	itext t 1 "c `cont` f = \\k -> c (\\a -> f a k)"
	text t "", \t -> do
	semititle t "h' x (\\y -> g' y (\\z -> f' z id))", \t -> do
	dvArrowShort t
	semititle t "h' x `cont` g' `cont` f' $ id", \t -> do
	arrowIText t 2 "演習問題:ここの説明終了後に各自簡約せよ"
 ]

-- h' x `cont` g' `cont` f' $ id
-- (\k -> h' x (\y -> g' y k)) `cont` f' $ id
-- \k' -> (\k -> h' x (\y -> g' y k)) (\z -> f' z k') $ id
-- \k' -> h' x (\y -> g' y (\z -> f' z k')) $ id
-- h' x (\y -> g' y (\z -> f' z id))

contMonad4 :: Page
contMonad4 = [\t -> do
	writeTopTitle t "Continuationモナド"
	text t "", \t -> do
	text t "* 名前をつければ制御可能となる", \t -> do
	arrowIText t 1 "引数となれば制御可能となる", \t -> do
	text t "* 試してみよう", \t -> do
	itext t 1 "h' x `cont` (\\y _ -> j' y) `cont` g' `cont` f'", \t -> do
	itext t 1 "- h xはgやfは無視してjにわたされる", \t -> do
	itext t 1 "- f(g(h(x)))であれば不可能なこと", \t -> do
	text t "* 何がうれしいの?", \t -> do
	itext t 1 "- C言語等のbreakに相当する機能を作ることができる"
 ]

contMonad5 :: Page
contMonad5 = [\t -> do
	writeTopTitle t "Continuationモナド"
	text t "", \t -> do
	text t "* 今まで見てきたもののモナド的な側面を見ていこう", \t -> do
	text t "* まずは型を見ていこう", \t -> do
	itext t 1 "h :: a -> bのときh' :: a -> (b -> c) -> c", \t -> do
	itext t 1 "c `cont` f = \\k -> c (\\a -> f a k)から"
	itext t 1 "cont :: ((a -> r) -> r) -> (a -> (b -> r) -> r)"
	itext t 4 "-> ((b -> r) -> r)", \t -> do
	text t "* 型シノニムを定義する", \t -> do
	itext t 1 "type Cont r a = (a -> r) -> r", \t -> do
	itext t 1 "cont :: Cont a -> (a -> Cont b) -> Cont b", \t -> do
	text t "* よってcontは>>=であることがわかる"
 ]

contMonad6 :: Page
contMonad6 = [\t -> do
	writeTopTitle t "Continuationモナド"
	text t "", \t -> do
	text t "* returnと>>=の定義を見てみよう", \t -> do
	itext t 1 "return x = \\k -> k x"
	itext t 1 "c >>= f = \\k -> c (\\a -> f a k)"
 ]

contMonadN :: Page
contMonadN = [\t -> do
	writeTopTitle t "Continuationモナド"
	text t "", \t -> do
	text t "* f(g(h(x)))の例を書き換えてみる", \t -> do
	itext t 1 "h' x (\\y -> g' y (\\z -> f' z id))"
	itext t 1 "", \t -> do
	itext t 1 "h' :: a -> (b -> c) -> c"
	itext t 1 "h' x = \\k -> k (h x)"
	itext t 1 "", \t -> do
	itext t 1 "g' :: b -> (d -> c) -> c"
	itext t 1 "g' x = \\k -> k (g x)"
	itext t 1 "", \t -> do
	itext t 1 "f' :: d -> (e -> c) -> c"
	itext t 1 "f' x = \\k -> k (f x)"
 ]

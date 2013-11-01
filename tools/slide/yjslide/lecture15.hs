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
	monadList1 4,
	monadList1 5,
	monadList1 6,
	monadList1 7,
	monadList1 8
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
	"Maybe", "Error", "List", "State", "Reader", "Writer",
	"Cont", "IO"]

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


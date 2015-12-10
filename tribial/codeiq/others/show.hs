import Control.Applicative
import Data.Maybe
import Data.List
import Data.Bits
import Data.Word
import Numeric

main :: IO ()
main = interact $ (++ "\n") . intercalate "," . map show
	. (<$> [tetI, tetL, tetO, tetS, tetT]) . flip checkField . field

checkField :: Tetromino -> Field -> Int
checkField t = length . filter isJust . (<$> putAllRots t) . overlap

overlap :: Field -> Field -> Maybe Field
overlap = (sequence .) . zipWith ((sequence .) . zipWith ol)
	where
	ol True True = Nothing; ol False False = Just False; ol _ _ = Just True

type Field = [[Bool]]

field :: String -> Field
field = map ((<$> [7, 6 .. 0]) . testBit . rh) . words
	where rh = fst . head . readHex :: String -> Word8

type Tetromino = [[Bool]]

tetI, tetL, tetO, tetS, tetT :: Tetromino
[tetI, tetL, tetO, tetS, tetT] = ((rt <$>) <$>) <$> [
	["****"], ["*..","***"], ["**", "**"], [".**", "**."], ["***", ".*."] ]
	where rt '*' = True; rt '.' = False; rt _ = error "never occur"

putAllRots :: Tetromino -> [Field]
putAllRots = (>>= putAll) . rotates

putAll :: Tetromino -> [Field]
putAll t = catMaybes [ put y x t | y <- [0 .. 7], x <- [0 .. 7] ]

put :: Int -> Int -> Tetromino -> Maybe Field
put y x t = padding 8 (replicate 8 False) =<< sequence (
	map Just (replicate y $ replicate 8 False) ++
	map (padding 8 False . (replicate x False ++)) t )
	where padding n x0 xs
		| l > n = Nothing
		| otherwise = Just $ xs ++ replicate (n - length xs) x0
		where l = length xs

rotates :: Tetromino -> [Tetromino]
rotates = nub . (>>= ((<$> [id, rotR, rotR . rotR, rotL])) . flip ($))
	. ((<$> [id, reverse]) . flip ($))
	where rotR = transpose . reverse; rotL = reverse . transpose

{-

[感想]
入力の形式がきれいな点が気に入った。
この解答は総当たりなのでもっと賢い方法があるのかもしれないとも思う。

[工夫した点]

1.
複雑なところは「データ」で表現し「動作」の部分を単純にした。
各テトロミノを空の8x8のフィールドに置く置きかたすべてのリストを作成した。
これらの全「置きかた」リストと与えられたフィールドとの重ね合わせを試し
可能なものだけろ過(filter)しその数を数えた。
全「置きかた」リストはキャッシュされる。

2.
テトロミノの置きかたを単に数えるだけでなく、
置いたあとのフィールドの状態を入手したいときにも、
最小限のコードの変更で可能なようにしておいた。

3. 「意味のある」単位で関数を切り分けることでコードを読みやすくした。

4. アプリカティブスタイルを使用しコードを簡潔にまとめた。

-}

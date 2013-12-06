import Lecture

subtitle :: String
subtitle = "第38回 MultiParamTypeClasses拡張"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	mkPack, mkPack2, mkPack3
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 複数の型をまきこんだクラスを定義することができる", \t -> do
	text t "* そのためにはMultiParamTypeClasses拡張が必要", \t -> do
	text t "* 型同士の関係を表していると考えることもできる"
 ]

mkPack :: Page
mkPack = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* Bool型のリストをWord64型のリストにすると", \t -> do
	itext t 1 "- メモリ使用量の削減", \t -> do
	itext t 1 "- ランダムアクセスの速度向上", \t -> do
	text t "* これをpackする呼ぶことにする", \t -> do
	text t "* Bool型とWord64型にしぼってしまうと以下に対応できない", \t -> do
	itext t 1 "- Bool型以外の型をpackしたい", \t -> do
	itext t 1 "- Word64型以外の型にpackしたい", \t -> do
	text t "* 複数の型をまきこんだクラスであれば", \t -> do
	itext t 1 "- 様々なペアでpack, unpackが可能"
 ]

mkPack2 :: Page
mkPack2 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* クラスを定義する", \t -> do
	itext t 1 "class Pack c e where"
	itext t 2 "pack :: [e] -> [c]"
	itext t 2 "unpack :: [c] -> [e]"
	itext t 2 "index :: [c] -> Int -> e", \t -> do
	text t "* 汎用のboolsToWordを用意する", \t -> do
	itext t 1 "boolsToWord :: (Bits w, Integral w) => [Bool] -> w"
	itext t 1 "boolsToWord [] = 0"
	itext t 1 "boolsToWord (b : bs) = fromIntegral (fromEnum b)"
	itext t 2 ".|. (boolsToWord bs) `shiftL` 1"
 ]

mkPack3 :: Page
mkPack3 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* 汎用のwordToBoolsを用意する", \t -> do
	itext t 0 "wordToBools ::"
	itext t 1 "(Bits w, Integral w) => Int -> w -> [Bool]"
	itext t 0 "wordToBools s w = bs ++ replicate (s - length bs) False"
	itext t 1 "where"
	itext t 1 "bs = wtb w"
	itext t 1 "wtb 0 = []"
	itext t 1 "wtb w' = toEnum (fromIntegral (w' .&. 1)) :"
	itext t 6 "wtb (w' `shiftR` 1)"
 ]

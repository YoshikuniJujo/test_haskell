import Lecture

subtitle :: String
subtitle = "第38回 MultiParamTypeClasses拡張"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude,
	mkPack, mkPack2, mkPack3, mkPack4, mkPack5, mkPack6, mkPack7,
	mkPack8, mkPack9, mkPack10, mkPack11, mkPack12, mkPack13, mkPack14,
	summary
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

mkPack4 :: Page
mkPack4 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* 汎用のindexWBを用意する", \t -> do
	itext t 1 "indexWB ::"
	itext t 2 "(Bits w, Num w) => Int -> [w] -> Int -> Bool"
	itext t 1 "indexWB s (w : ws) i"
	itext t 2 "| i < s = w .&. (1 `shiftL` i) /= 0"
	itext t 2 "| otherwise = indexWB s ws (i - s)"
 ]

mkPack5 :: Page
mkPack5 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* BoolのリストをWord8のリストにpackする", \t -> do
	itext t 1 "instance Pack Word8 Bool where"
	itext t 2 "pack [] = []"
	itext t 2 "pack bs = boolsToWord (take 8 bs) :"
	itext t 6 "pack (drop 8 bs)"
	itext t 2 "unpack = concatMap (wordToBool 8)"
	itext t 2 "index = indexWB 8"
 ]

mkPack6 :: Page
mkPack6 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* BoolのリストをCharのリストにpackする", \t -> do
	itext t 1 "instance Pack Char Bool where"
	itext t 2 "pack = map (chr . fromIntegral) ."
	itext t 4 "(pack :: [Bool] -> [Word8])"
	itext t 2 "unpack = (unpack :: [Word8] -> [Bool]) ."
	itext t 4 "map (fromIntegral . ord)"
	itext t 2 "index = indexWB 8 . map ord"
 ]

mkPack7 :: Page
mkPack7 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* BoolのリストをWord64のリストにpackする", \t -> do
	itext t 1 "instance Pack Word64 Bool where"
	itext t 2 "pack [] = []"
	itext t 2 "pack bs = boolsToWord (take 64 bs) :"
	itext t 6 "pack (drop 64 bs)"
	itext t 2 "unpack = concatMap (wordToBool 64)"
	itext t 2 "index = indexWB 64"
 ]

mkPack8 :: Page
mkPack8 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* テスト用のファイルを作成", \t -> do
	itext t 0 "testData :: String"
	itext t 0 "testData = concat $ replicate (10 ^ 3) \"1234567890\""
	itext t 0 "main :: IO ()"
	itext t 0 "main = writeFile \"10kFile.txt\" testData", \t -> do
	text t "* テスト用のファイルからBool値の生成", \t -> do
	itext t 0 "getTestBools :: IO [Bool]"
	itext t 0 "getTestBools = do"
	itext t 1 "cnt <- readFile \"10kFile.txt\""
	itext t 1 "return $ unpack cnt"
 ]

mkPack9 :: Page
mkPack9 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* packしないブール値のリストに対するランダムアクセス", \t -> do
	itext t 1 "randomAccess :: Int -> [Bool] -> IO Bool"
	itext t 1 "randomAccess len bs = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "return $ bs !! i"
	itext t 1 "main :: IO ()"
	itext t 2 "bs <- getTestBools"
	itext t 2 "(10 ^ 5) `timesDo` (do"
	itext t 3 "b <- randomAccess (8 * 10 ^ 4) bs"
	itext t 3 "putStr $ show b)"
	itext t 2 "putChar '\\n'"
 ]

mkPack10 :: Page
mkPack10 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* かかった時間は26.79秒", \t -> do
	text t "* メモリの使用パターンを次に示す", \t -> do
	itext t 1 "- なめらかに下降する", \t -> do
	itext t 1 "- Charからの変換が遅延しているため"
 ]

mkPack11 :: Page
mkPack11 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/pack/notUsePack.png"), \t -> do
	text t "* 最終的には1.5MBほどになっている"
 ]

mkPack12 :: Page
mkPack12 = [\t -> do
	writeTopTitle t "Packの例", \t -> do
	text t "* packを利用した場合", \t -> do
	itext t 1 "randomAccess :: Int -> [Word64] -> IO Bool"
	itext t 1 "randomAccess len bs = do"
	itext t 2 "i <- randomRIO (0, len - 1)"
	itext t 2 "return $ bs `index` i"
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "bs <- getTestBools"
	itext t 2 "let packed = pack bs"
	itext t 2 "(10 ^ 5) `timesDo` (do"
	itext t 3 "b <- randomAccess (8 * 10 ^ 4) packed"
	itext t 3 "putStr $ show b)"
	itext t 2 "putChar '\\n'"
 ]

mkPack13 :: Page
mkPack13 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	text t "* かかった時間は5.42秒", \t -> do
	itext t 1 "- packを使わなかったときは26.79秒なので", \t -> do
	itext t 1 "- 5倍ほどの速度となっている", \t -> do
	text t "* メモリの使用パターンは", \t -> do
	itext t 1 "- 急速に下降している", \t -> do
	itext t 1 "- Word64へのpackがうまくいっている"
 ]

mkPack14 :: Page
mkPack14 = [\t -> do
	writeTopTitle t "Packの例"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/pack/usePack.png"), \t -> do
	text t "* 最終的には80kBほどになっている", \t -> do
	text t "* 読み込んだデータが80kBなので予想通り", \t -> do
	itext t 1 "- packしなかったときは1.5MB", \t -> do
	itext t 1 "- 20倍ほどの効率となっている"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* MultiParamTypeClasses拡張について学んだ", \t -> do
	text t "* 複数の型を取ることのできるクラスを定義できる", \t -> do
	text t "* 単一の型を取るクラスは型が共有する性質と考えられる", \t -> do
	text t "* 複数の型を取るクラスは型が共有する関係と考えられる", \t -> do
	text t "* ある程度意味のある例としてPackの例を挙げた", \t -> do
	itext t 1 "- 原理的には遅延ByteStringによる最適化と同じ"
 ]

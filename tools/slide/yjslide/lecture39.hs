import Lecture

subtitle :: String
subtitle = "第40回 FlexibleInstances拡張"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	errorSample, errorSample2, errorSample3, errorSampleSummary,
	rev, rev2, rev3, revSummary,
--	pair, pair2, pair3, pair4, pair5, pair6, pair7, pair8, pair9,
--	pair10, pair11, pairSummary,
	flexibleContexts, flexibleContexts2,
	newPair, newPair2, newPair3, newPair4, newPair5, newPair6, newPair7,
	newPair8, newPair9, newPairSummary,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* FlexibleInstancesという言語拡張がある", \t -> do
	text t "* Haskellの標準ではinstance宣言には以下の制限がある", \t -> do
	itext t 1 "instance Class (Cons x1 x2 ... xk) where", \t -> do
	itext t 1 "- x1からxkは型「変数」であること", \t -> do
	itext t 1 "- x1からxkは互いに「異なる」型変数であること", \t -> do
	text t "* この制限はおそらくinstance宣言の重複を避けるため", \t -> do
	itext t 1 "instance Class [Char] where", \t -> do
	itext t 1 "instance Class [a] where", \t -> do
	itext t 1 "- 上記2つのinstance宣言は重複している"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 重複の別の例", \t -> do
	itext t 1 "instance Class (Either a a) where", \t -> do
	itext t 1 "instance Class (Either a b) where", \t -> do
	itext t 1 "- これらも重複している", \t -> do
	text t "* x1からxkに型構築子を許すと便利なときがある", \t -> do
	itext t 1 "instance ErrorMonad (Either String) where", \t -> do
	text t "* x1からxkに同じ型変数を許すと便利なときがある", \t -> do
	text t "* instanceになる型自体を型変数にすると便利なときがある", \t -> do
	itext t 1 "- MultiParamTypeClasses拡張と一緒に使う場合等"
 ]

errorSample :: Page
errorSample = [\t -> do
	writeTopTitle t "エラーを扱う"
	text t "", \t -> do
	text t "* エラーを扱う例", \t -> do
	itext t 1 "class Monad m => ErrorMonad m where"
	itext t 2 "throw :: String -> m a"
	itext t 2 "catch :: m a -> (String -> m a) -> m a", \t -> do
	text t "* 引数の順を変えた関数があると便利", \t -> do
	itext t 1 "handle :: ErrorMonad m =>"
	itext t 2 "(String -> m a) -> m a -> m a"
	itext t 1 "handle = flip catch"
 ]

errorSample2 :: Page
errorSample2 = [\t -> do
	writeTopTitle t "エラーを扱う"
	text t "", \t -> do
	text t "* Either StringをErrorMonadとして使う", \t -> do
	itext t 1 "instance ErrorMonad (Either String) where"
	itext t 2 "throw = Left"
	itext t 2 "catch (Left s) handler = handler s"
	itext t 2 "catch r _ = r", \t -> do
	text t "* 安全なdivを作る", \t -> do
	itext t 1 "safeDiv :: Int -> Int -> Either String Int"
	itext t 1 "_ `safeDiv` 0 = throw \"can't divide by 0\""
	itext t 1 "x `safeDiv` y = return $ x `div` y"
 ]

errorSample3 :: Page
errorSample3 = [\t -> do
	writeTopTitle t "エラーを扱う"
	text t "", \t -> do
	text t "* divの結果をメッセージとして返す", \t -> do
	itext t 1 "divMsg :: Int -> Int -> Either String String"
	itext t 1 "divMsg x y ="
	itext t 2 "handle (return . (\"*** Error: \" ++)) $ do"
	itext t 3 "d <- x `safeDiv` y"
	itext t 3 "return $ \"The answer is \" ++ show d", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "> divMsg 8 3"
	itext t 1 "Right \"The answer is 2\"", \t -> do
	itext t 1 "> divMsg 11 0"
	itext t 1 "Right \"*** Error: can't divide by 0\""
 ]

errorSampleSummary :: Page
errorSampleSummary = [\t -> do
	writeTopTitle t "エラーを扱う(まとめ)"
	text t "", \t -> do
	text t "* instance宣言のなかの型変数を型構築子に変えられる", \t -> do
	text t "* それが役に立つ例を見た", \t -> do
	text t "* 次は同じ型変数を2回以上使う例を見よう"
 ]

rev :: Page
rev = [\t -> do
	writeTopTitle t "裏返す"
	text t "", \t -> do
	text t "* 人工的な例", \t -> do
	itext t 1 "class Reversible a where"
	itext t 2 "rev :: a -> a", \t -> do
	text t "* リストは裏返せる", \t -> do
	itext t 1 "instance Reversible [a] where"
	itext t 2 "rev = reverse", \t -> do
	text t "* Either a b型もaとbが同じ型なら裏返せる", \t -> do
	itext t 1 "instance Reversible (Either a a) where"
	itext t 2 "rev (Left x) = Right x"
	itext t 2 "rev (Right x) = Left x"
 ]

rev2 :: Page
rev2 = [\t -> do
	writeTopTitle t "裏返す", \t -> do
	text t "* (a, b)型もaとbが同じ型なら裏返せる", \t -> do
	itext t 1 "instance Reversible (a, a) where"
	itext t 2 "rev (x, y) = (y, x)", \t -> do
	text t "* 同様にタプルは裏返せる", \t -> do
	itext t 1 "instance Reversible (a, b, a) where"
	itext t 2 "rev (x, y, z) = (z, y, x)", \t -> do
	itext t 1 "instance Reversible (a, b, b, a) where"
	itext t 2 "rev (x, y, z, w) = (w, z, y, x)", \t -> do
	itext t 1 "instance Reversible (a, b, c, b, a) where"
	itext t 2 "rev (x, y, z, w, v) = (v, w, z, y, x)", \t -> do
	itext t 1 "instance Reversible (a, b, c, c, b, a) where"
	itext t 2 "rev (x, y, z, w, v, u) = (u, v, w, z, y, x)"
 ]

rev3 :: Page
rev3 = [\t -> do
	writeTopTitle t "裏返す"
	text t "", \t -> do
	text t "* 裏返しの前後で値が同じなら回文構造である", \t -> do
	itext t 1 "isPalindrome :: (Rev a, Eq a) => a -> Bool"
	itext t 1 "isPalindrome x = x == rev x", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "> isPalindrome \"amoreroma\""
	itext t 1 "True", \t -> do
	itext t 1 "> isPalindrome \"amoretokyo\""
	itext t 1 "False", \t -> do
	itext t 1 "> isPalindrome ((3.1, 8, 8, 3.1) ::"
	itext t 4 "(Double, Int, Int, Double)"
	itext t 1 "True"
 ]

revSummary :: Page
revSummary = [\t -> do
	writeTopTitle t "裏返す(まとめ)"
	text t "", \t -> do
	text t "* 実用性のあまりない例", \t -> do
	text t "* リストとタプルの両方で使える回文チェッカーを作った", \t -> do
	text t "* 続く例ではより実用的なものを示す", \t -> do
	itext t 1 "- MultiParamTypeClassesと組み合わせる"
 ]

pair :: Page
pair = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* 非負整数のタプルの代わりにWord64を使うと", \t -> do
	itext t 1 "- 空間効率が上昇することが期待できる", \t -> do
	text t "* プログラムの本体を変えずに2つの表現を切り替える", \t -> do
	itext t 1 "- 型宣言を変えるだけで切り替え可能にする", \t -> do
	text t "* Pairableクラスの定義", \t -> do
	itext t 1 "class Pairable p e where"
	itext t 2 "pair :: e -> e -> p"
	itext t 2 "unpair :: p -> (e, e)"
 ]

pair2 :: Page
pair2 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* ペアの表現に以下の2つを使おうとしている", \t -> do
	itext t 1 "- (a, a)", \t -> do
	itext t 1 "- Word64", \t -> do
	text t "* 問題は両者に対する制約の違い", \t -> do
	itext t 1 "- (a, a)はa型のペアであれば何でも定義可能", \t -> do
	itext t 1 "- Word64は非負整数のペアのみを表現する"
 ]

pair3 :: Page
pair3 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* (a, a)をPairableクラスのインスタンスにする", \t -> do
	itext t 1 "instance Pairable (e, e) e where"
	itext t 2 "pair = (,)"
	itext t 2 "unpair = id", \t -> do
	itext t 1 "- 3つのeによって型をそろえている", \t -> do
	itext t 1 "- これで型さえ合えばどんな型でもペアにできる"
 ]

pair4 :: Page
pair4 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* Word64をPairableクラスのインスタンスにする", \t -> do
	text t "* 要素の型はひとつずつ指定してやる必要がある", \t -> do
	itext t 0 "instance Pairable Word64 Word8 where"
	itext t 1 "pair w1 w2 = fromIntegral w1 `shiftL` 8 .|."
	itext t 6 "fromIntegral w2"
	itext t 1 "unpair w = (fromIntegral $ w `shiftR` 8,"
	itext t 5 "fromIntegral $ w .&. 0xff)"
 ]

pair5 :: Page
pair5 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* Word16, Word32についても同様の定義", \t -> do
	itext t 0 "instance Pairable Word64 Word16 where"
	itext t 1 "pair w1 w2 = fromIntegral w1 `shiftL` 16 .|."
	itext t 6 "fromIntegral w2"
	itext t 1 "unpair w = (fromIntegral $ w `shiftR` 16,"
	itext t 4 "fromIntegral $ w .&. 0xffff)", \t -> do
	itext t 0 "instance Pairable Word64 Word32 where"
	itext t 1 "pair w1 w2 = fromIntegral w1 `shiftL` 32 .|."
	itext t 6 "fromIntegral w2"
	itext t 1 "unpair w = (fromIntegral $ w `shiftR` 32,"
	itext t 4 "fromIntegral $ w .&. 0xffffffff)"
 ]

pair6 :: Page
pair6 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* ペアとしてタプルを使う場合", \t -> do
	itext t 1 "type WordPair = (Word32, Word32)", \t -> do
	text t "* ランダムなペアのリストを作る", \t -> do
	itext t 1 "getRandomPairs :: IO [WordPair]"
	itext t 1 "getRandomPairs = replicateM (10 ^ 4) $ do"
	itext t 2 "w1 <- randomIO"
	itext t 2 "w2 <- randomIO"
	itext t 2 "return $ pair w1 w2"
 ]

pair7 :: Page
pair7 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* マージンつきの検索関数", \t -> do
	itext t 1 "lookupPair ::"
	itext t 2 "Word32 -> [WordPair] -> Maybe Word32"
	itext t 1 "lookupPair w0 (wp : wps)"
	itext t 2 "| (w1, w2) <- unpair wp,"
	itext t 3 "abs (w1 - w0) < 1000 = Just w2"
	itext t 2 "| otherwise = lookupPair w0 wps"
 ]

pair8 :: Page
pair8 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* main関数", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = do"
	itext t 2 "ps <- getRandomPairs"
	itext t 2 "(10 ^ 3) `timesDo` (do"
	itext t 3 "k <- randomIO"
	itext t 3 "case lookupPair k ps of"
	itext t 4 "Just v -> print v"
	itext t 4 "_ -> return ())"
 ]

pair9 :: Page
pair9 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/pair/lookupTuple.png"), \t -> do
	text t "* 2.2MB程度のメモリを使用している"
 ]

pair10 :: Page
pair10 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	text t "* ペアとしてWord64を使う場合", \t -> do
	itext t 1 "type WordPair = Word64", \t -> do
	itext t 1 "- 変更点はここだけで良い", \t -> do
	text t "* この場合のメモリ使用状況も見てみよう"
 ]

pair11 :: Page
pair11 = [\t -> do
	writeTopTitle t "ペア"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/pair/lookupWord64.png"), \t -> do
	text t "* 260kB程度のメモリを使用している", \t -> do
	itext t 1 "- タプルを使った場合は2.2MBほど", \t -> do
	itext t 1 "- 8倍程度の空間効率の向上"
 ]

pairSummary :: Page
pairSummary = [\t -> do
	writeTopTitle t "ペア(まとめ)"
	text t "", \t -> do
	text t "* 型を合わせれば全ての型で使えるペアの表現がある", \t -> do
	text t "* 限られた型にしか使えないペアの表現もある", \t -> do
	text t "* 両者を同じクラスのインスタンスとして表現できる", \t -> do
	itext t 1 "- MultiParamTypeClasses拡張が必要", \t -> do
	itext t 1 "- FlexibleInstances拡張が必要"
 ]

flexibleContexts :: Page
flexibleContexts = [\t -> do
	writeTopTitle t "FlexibleContexts"
	text t "", \t -> do
	text t "* FlexibleContextsという言語拡張がある", \t -> do
	text t "* 標準ではContextはClass varという形", \t -> do
	text t "* 複数引数クラスに対してはClass var1 var2 ...という形", \t -> do
	text t "* いずれにしてもクラスに与えられる引数は単純な型変数", \t -> do
	text t "* FlexibleInstances拡張なしで作られたクラスには十分", \t -> do
	itext t 1 "- インスタンス定義にCons v1 v2 ...という形", \t -> do
	itext t 1 "- Cons ...がクラスに属するかどうかは", \t -> do
	itext t 2 "v1, v2 ...に対する文脈のみで定まる"
 ]

flexibleContexts2 :: Page
flexibleContexts2 = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* FlexibleInstances拡張のもとで作られたクラスには", \t -> do
	itext t 1 "- Cons v1 v2 ... C1 ... C2 ... vkという形がある", \t -> do
	itext t 1 "- Cons v1 v2 ... vi ... vi ... vkという形もある", \t -> do
	arrowIText t 1 "Cons ...がクラスに属するかどうかは", \t -> do
	itext t 2 "v1, v2 ...に対する文脈のみでは定まらない", \t -> do
	text t "* Class (Cons ...)という形の文脈が必要になってくる"
 ]

newPair :: Page
newPair = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	text t "* MultiParamTypeClasses拡張と一緒に使う場合", \t -> do
	text t "* instance宣言の型を型変数にすることにも意味が出てくる", \t -> do
	itext t 1 "instance Class Type a", \t -> do
	text t "* この例について見ていこう", \t -> do
	itext t 1 "class Pairable p e where"
	itext t 2 "pair :: e -> e -> p e"
	itext t 2 "unpair :: p e -> (e, e)", \t -> do
	itext t 1 "newtype TPair a = TPair (a, a)", \t -> do
	itext t 1 "newtype WPair a = WPair Word64"
 ]

newPair2 :: Page
newPair2 = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	text t "* TPairのinstance宣言", \t -> do
	itext t 0 "instance Pairable TPair e where"
	itext t 1 "pair x y = TPair (x, y)"
	itext t 1 "unpair (TPair p) = p", \t -> do
	text t "* WPairについては要素ごとにinstance宣言する", \t -> do
	itext t 0 "instance Pairable WPair Word8 where"
	itext t 1 "pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 8)"
	itext t 5 ".|. fromIntegral w2"
	itext t 1 "unpair (WPair w) = (fromIntegral $ w `shiftR` 8,"
	itext t 5 "fromIntegral $ w .&. 0xff)"
 ]

newPair3 :: Page
newPair3 = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	text t "* WPairについてWord16, Word32についても同様", \t -> do
	itext t 0 "instance Pairable WPair Word16 where"
	itext t 1 "pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 16)"
	itext t 5 ".|. fromIntegral w2"
	itext t 1 "unpair (WPair w) = (fromIntegral $ w `shiftR` 16,"
	itext t 4 "fromIntegral $ w .&. 0xffff)", \t -> do
	itext t 0 "instance Pairable WPair Word32 where"
	itext t 1 "pair w1 w2 = WPair $ (fromIntegral w1 `shiftL` 32)"
	itext t 5 ".|. fromIntegral w2"
	itext t 1 "unpair (WPair w) = (fromIntegral $ w `shiftR` 32,"
	itext t 4 "fromIntegral $ w .&. 0xffffffff)"
 ]

newPair4 :: Page
newPair4 = [\t -> do
	writeTopTitle t "ペアの別の例", \t -> do
	text t "* テスト用の関数(FlexibleContextsが必要)", \t -> do
	itext t 0 "getRandomPairs :: Pairable p Word32 => IO [p Word32]"
	itext t 0 "getRandomPairs = replicateM (10 ^ 4) $ do"
	itext t 1 "w1 <- randomIO"
	itext t 1 "w2 <- randomIO"
	itext t 1 "return $ pair w1 w2", \t -> do
	itext t 0 "lookupPair :: Pairable p Word32 =>"
	itext t 1 "Word32 -> [p Word32] -> Maybe Word32"
	itext t 0 "lookupPair w0 (wp : wps)"
	itext t 1 "| (w1, w2) <- unpair wp,"
	itext t 2 "abs (w1 - w0) < 1000 = Just w2"
	itext t 1 "| otherwise = lookupPair w0 wps"
 ]

newPair5 :: Page
newPair5 = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	text t "* テスト用のmain", \t -> do
	itext t 1 "main :: Pairable p Word32 => p Word32 -> IO ()"
	itext t 1 "main (_ :: p Word32) = do"
	itext t 2 "ps <- getRandomPairs :: IO [p Word32]"
	itext t 2 "(10 ^ 3) `timesDo` (do"
	itext t 3 "k <- randomIO"
	itext t 3 "case lookupPair k ps of"
	itext t 4 "Just v -> print v"
	itext t 4 "_ -> return ())"
 ]

newPair6 :: Page
newPair6 = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	text t "* タプルを使う場合", \t -> do
	itext t 1 "main = T.main (undefined :: TPair Word32)", \t -> do
	text t "* このときのメモリの使用状況は"
 ]

newPair7 :: Page
newPair7 = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/pair2/lookupTuple.png"), \t -> do
	text t "* 2.2MB程度のメモリを使用している"
 ]

newPair8 :: Page
newPair8 = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	text t "* Word64をペアに使う場合", \t -> do
	itext t 1 "main :: IO ()"
	itext t 1 "main = T.main (undefined :: WPair Word64)", \t -> do
	text t "* メモリ使用状況は"
 ]

newPair9 :: Page
newPair9 = [\t -> do
	writeTopTitle t "ペアの別の例"
	text t "", \t -> do
	writeImageCenter t 82 (300, 180, "examples/pair2/lookupWord.png"), \t -> do
	text t "* 260kB程度のメモリを使用している", \t -> do
	itext t 1 "- タプルを使った場合は2.2MBほど", \t -> do
	itext t 1 "- 8倍程度の空間効率の向上"
 ]

newPairSummary :: Page
newPairSummary = [\t -> do
	writeTopTitle t "ペアの別の例(まとめ)"
	text t "", \t -> do
	text t "* 以下を同じクラスのインスタンスにすることができる", \t -> do
	itext t 1 "- すべての型に使えるコンテナ", \t -> do
	itext t 1 "- 特定の型にしか使えないコンテナ"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* FlexibleInstances拡張について学んだ", \t -> do
	text t "* 型変数の代わりに型が使える", \t -> do
	text t "* 同じ型変数の2回以上の出現を許す", \t -> do
	text t "* 型の代わりに型変数が使える", \t -> do
	text t "* 後者はMultiParamTypeClasses拡張と組み合わせることで", \t -> do
	itext t 1 "- 以下を同じクラスのインスタンスとして表現可能", \t -> do
	itext t 2 "全ての型で使える構造", \t -> do
	itext t 2 "特定の型でしか使えない構造", \t -> do
	text t "* この機構は次回に学ぶMArrayクラスに使われている", \t -> do
	itext t 1 "- IOArrayとIOUArrayが同じクラスのインスタンスに"
 ]

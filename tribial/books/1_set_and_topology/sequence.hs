import Data.List

data Seq1 = Seq1 (Integer -> Double)
data Seq2 = Seq2 [Double]

seq12 :: Seq1 -> Seq2
seq12 (Seq1 f) = Seq2 $ map f [1 ..]

showSeq2 :: Seq2 -> String
showSeq2 (Seq2 s) = intercalate ", " (map show $ take 10 s) ++ "..."

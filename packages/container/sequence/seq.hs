import qualified Data.Sequence as Seq
import System.Random
import Data.Time

main :: IO ()
main = do
	g <- getStdGen
	let rs = randomRs (0, 2 ^ 15 - 1) g
	t0 <- getCurrentTime
	print . sum . map (sampleList !!) $ take 100000 rs
	t1 <- getCurrentTime
	print $ t1 `diffUTCTime` t0
	print . sum . map (sampleSeq `Seq.index`) $ take 100000 rs
	t2 <- getCurrentTime
	print $ t2 `diffUTCTime` t1

sampleList :: [Int]
sampleList = [0 .. 2 ^ 15 - 1]

sampleSeq :: Seq.Seq Int
sampleSeq = Seq.fromList sampleList

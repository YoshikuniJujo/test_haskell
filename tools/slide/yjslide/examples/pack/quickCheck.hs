import Test.QuickCheck
import TestBools
import Pack
import System.IO.Unsafe
import Data.Word
import Control.Applicative

bools :: [Bool]
bools = unsafePerformIO getTestBools

packed :: [Word64]
packed = pack bools

-- check :: Int -> Bool
check i = i >= 0 && i <= 79999 ==> bools !! i == packed `index` i

checkGen i = bools !! i == packed `index` i
check2 = checkGen <$> elements [0 .. 79999]

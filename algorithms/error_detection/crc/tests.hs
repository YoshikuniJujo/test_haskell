import Data.Bits
import Data.Bool

litBinary, bigBinary :: String -> Int
litBinary = foldr (\c n -> bool 0 1 (c == '1') .|. n `shiftL` 1) 0

bigBinary = litBinary . reverse

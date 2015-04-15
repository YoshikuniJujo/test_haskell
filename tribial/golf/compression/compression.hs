import Control.Applicative
import Data.List

main :: IO ()
main = interact $ concatMap showCompression . compression

compression :: String -> [(Char, Int)]
compression str = map ((,) <$> head <*> length) $ group str

showCompression :: (Char, Int) -> String
showCompression (c, 1) = [c]
showCompression (c, n) = c : show n

import Control.Applicative
import Data.Word
import qualified Data.ByteString as BS
import System.Environment
import System.FilePath
import Numeric

import Codec

main :: IO ()
main = do
	fp : _ <- getArgs
	maybe (return ()) (BS.writeFile (replaceExtension fp "png")) . png . lines
		=<< readFile fp

png :: [String] -> Maybe BS.ByteString
png tx = encode . uncurry (uncurry mkImage) <$> readText tx

readText :: [String] -> Maybe ((Int, Int), [BS.ByteString])
readText (wh : i) = Just (read $ '(' : wh ++ ")", map (BS.pack . toWords) i)
readText _ = Nothing

toWords :: String -> [Word8]
toWords (n1 : n2 : ns) = fst (head $ readHex [n1, n2]) : toWords ns
toWords _ = []

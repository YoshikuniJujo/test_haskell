import Data.Traversable
import Control.Applicative
-- import Control.Monad
import Data.List

fun :: Int -> Bool -> Char -> String
fun i b c = intercalate " " [show i, show b, show c]

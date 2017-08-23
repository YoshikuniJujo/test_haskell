import Control.Arrow
import Data.Char
import Text.Read

fun :: String -> (Maybe Integer, String)
fun = first readMaybe . span isDigit

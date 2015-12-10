import Control.Applicative
import System.Environment

main :: IO ()
main = putStr =<< addR <$> (readFile . head =<< getArgs)

addR :: String -> String
addR "" = ""
addR ('\r' : '\n' : cs) = '\r' : '\n' : addR cs
addR ('\n' : cs) = '\r' : '\n' : addR cs
addR (c : cs) = c : addR cs

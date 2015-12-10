import Control.Applicative
import Data.List
import Data.Function

main :: IO ()
main = interact $ (++ "\n") . blockSort . chop

rotates, rotatesForever :: [a] -> [[a]]
rotates = take <$> length <*> rotatesForever
rotatesForever xa = xa : rotates (last xa : init xa)

blockSort :: Ord a => [a] -> [a]
blockSort = map last . sort . rotates

chop :: String -> String
chop "" = ""
chop "\n" = ""
chop (c : cs) = c : chop cs

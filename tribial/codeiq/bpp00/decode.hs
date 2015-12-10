
main :: IO ()
main = interact $ (++ "\n") . concatMap (\(c, n) -> replicate n c) . toRL . chop

-- decode :: String -> String
-- decode

toRL :: String -> [(Char, Int)]
toRL "" = []
toRL (c : n : cns) = (c, read [n]) : toRL cns

chop :: String -> String
chop "\n" = ""
chop (c : cs) = c : chop cs

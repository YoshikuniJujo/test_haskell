table :: [(Char, Char)]
table = zip ['a' .. 'z'] (['h' .. 'z'] ++ ['a' .. 'g'])

crypt :: String -> Maybe String
crypt = mapM (`lookup` table)

decrypt :: String -> Maybe String
decrypt = mapM (`lookup` map (uncurry $ flip (,)) table)

table :: [(Char, Char)]
table = zip ['a' .. 'z'] (['d' .. 'z'] ++ ['a' .. 'c'])

crypt :: String -> Maybe String
crypt (c : cs) = (:) <$> lookup c table <*> crypt cs
crypt "" = Just ""

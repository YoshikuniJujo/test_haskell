table :: [(Char, Char)]
table = zip ['a' .. 'z'] (['d' .. 'z'] ++ ['a' .. 'c'])

ltraverse :: Applicative f => (a -> f b) -> [a] -> f [b]
ltraverse f (x : xs) = (:) <$> f x <*> ltraverse f xs
ltraverse _ _ = pure []

crypt :: String -> Maybe String
crypt = ltraverse (`lookup` table)

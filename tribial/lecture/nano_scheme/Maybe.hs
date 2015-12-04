module Maybe (mapply, mbind) where

mapply :: (a -> b) -> Maybe a -> Maybe b
mapply = maybe Nothing . (Just .)

mbind :: (a -> Maybe b) -> Maybe a -> Maybe b
mbind = maybe Nothing

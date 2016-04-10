
msequence :: Applicative m => Maybe (m a) -> m (Maybe a)
msequence Nothing = pure Nothing
msequence (Just m) = fmap Just m

lsequence :: Applicative m => [m a] -> m [a]
lsequence [] = pure []
lsequence (m : ms) = (:) <$> m <*> lsequence ms

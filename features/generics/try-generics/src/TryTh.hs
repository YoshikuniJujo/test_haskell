module TryTh where

import Language.Haskell.TH

tupT :: [Name] -> Type
tupT ns = foldl AppT (TupleT $ length ns) $ VarT <$> ns

tupTN :: Int -> Type
tupTN = tupT . nameN

nameN :: Int -> [Name]
nameN n = mkName . (: []) <$> take n ['a' .. 'z']

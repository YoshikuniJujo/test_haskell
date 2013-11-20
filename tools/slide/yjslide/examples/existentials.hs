-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

import Data.Typeable

data ShowBox = forall s . Show s => SB s

instance Show ShowBox where
	show (SB s) = show s

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

showSB :: ShowBox -> String
showSB (SB s) = show s

data Anything = forall t . Typeable t => Any t

{-
fromAnything' :: Typeable t => Anything -> t
fromAnything' (Any t) = t
-}

fromAnything :: Typeable t => Anything -> Maybe t
fromAnything (Any t) = cast t

typeOfAnything :: Anything -> TypeRep
typeOfAnything (Any t) = typeOf t

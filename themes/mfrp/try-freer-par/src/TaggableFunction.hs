{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TaggableFunction (TaggableFun) where

import Funable (Funable(..), Taggable(..), Id)

---------------------------------------------------------------------------

data TaggableFun m a b where
	Fun :: Maybe Id -> (a -> m b) -> TaggableFun m a b

instance Funable TaggableFun where fun = Fun Nothing; ($$) (Fun _ f) = f

instance Taggable TaggableFun where
	putTag t (Fun _ f) = Fun (Just t) f
	getTag (Fun t _) = t

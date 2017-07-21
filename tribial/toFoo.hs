{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.Maybe

data Foo p = NoFoo | YesFoo (forall a . p a)

fromFoo :: Foo p -> Maybe (p a)
fromFoo NoFoo = Nothing
fromFoo (YesFoo p) = Just p

-- toFoo :: forall p . (forall a . Maybe (p a)) -> Foo p
toFoo :: forall p . (Maybe (forall a . p a)) -> Foo p
toFoo m = case m :: Maybe (p ()) of
		Nothing -> NoFoo
		Just _ -> YesFoo (fromJust m)
-- toFoo = maybe NoFoo YesFoo

data Hoge a = Hoge | Fuga a deriving Show

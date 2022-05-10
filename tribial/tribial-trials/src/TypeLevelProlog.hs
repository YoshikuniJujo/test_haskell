{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevelProlog where

class Likes a b where likes :: ()
class Female a where female :: ()

data Taro
data Hanako

instance Likes Taro Hanako where likes = ()

data John
data Wine

instance Female Hanako where female = ()
instance Likes Hanako Wine where likes = ()

instance (Likes x Wine, Female x) => Likes John x where likes = ()

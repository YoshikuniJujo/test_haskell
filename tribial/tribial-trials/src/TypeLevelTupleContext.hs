{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevelTupleContext where

import Data.Kind

data Two a b = Two a b deriving Show

data Two' ab where Two' :: Two a b -> Two' '(a, b)

deriving instance (Show a, Show b) => Show (Two' '(a, b))

printOne :: Show a => Two a b -> IO ()
printOne (Two x _y) = print x

printOne' :: ShowOne ab => Two' ab -> IO ()
printOne' = po

class ShowOne (ab :: (Type, Type)) where
	po :: Two' ab -> IO ()

instance Show a => ShowOne '(a, b) where
	po (Two' t) = printOne t

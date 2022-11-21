{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevelTupleContext where

data Two ab where Two :: a -> b -> Two '(a, b)

deriving instance (Show a, Show b) => Show (Two '(a, b))

printOne :: ShowOne ab => Two ab -> IO ()
printOne t = putStrLn $ showOne t

class ShowOne ab where showOne :: Two ab -> String
instance Show a => ShowOne '(a, b) where showOne (Two x _y) = show x

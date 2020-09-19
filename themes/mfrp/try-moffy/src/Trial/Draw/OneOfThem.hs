{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw.OneOfThem where

import Data.Kind
import Data.Type.Set
import Data.OneOfThem

class SortType (as :: [Type]) (as' :: Set Type) where
	sortType :: [OneOfThem as'] -> [OneOfThem as']

instance SortType '[] as' where sortType _ = []

instance (Projectable as' a, SortType as as') => SortType (a ': as) as' where
	sortType xs = filter (\oot -> case project @as' @a oot of Just _ -> True; Nothing -> False) xs ++ sortType @as @as' xs

numbered [t| Bool |]
numbered [t| Char |]

sample :: [OneOfThem (Bool :- Char :- 'Nil)]
sample = True >- 'a' >- False >- 'b' >-  ([] :: [OneOfThem 'Nil])

showSample :: OneOfThemFun (Bool :- Char :- 'Nil) String
showSample = (show :: Bool -> String) >-- SingletonFun (show :: Char -> String)

-- instance 

-- sortType :: proxy '[Type] -> [OneOfThem as] -> [OneOfThem as]
-- sortType 

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeOperators, FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryContain where

class a `Contain` b | b -> a where
	withdraw :: a -> b

data Profile = Profile { name :: Name, age :: Age } deriving Show
data Name = Name { givenName :: GivenName, familyName :: FamilyName } deriving Show
newtype GivenName = GivenName String deriving Show
newtype FamilyName = FamilyName String deriving Show
newtype Age = Age Int deriving Show

instance Profile `Contain` Name where
	withdraw = name

instance Profile `Contain` Age where
	withdraw = age

instance Name `Contain` FamilyName where
	withdraw = familyName

instance Name `Contain` GivenName where
	withdraw = givenName

withdraw2 :: (Contain a b, Contain b c) => a -> c
withdraw2 = withdraw . withdraw

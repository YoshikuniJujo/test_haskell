{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Constraint where

-- data Bar a = Bar (Show a => Maybe a)
data Bar = forall a . Show a => Bar (Maybe a)

foo :: (forall a . Show a => Maybe a -> b) -> b
foo f = f (Just 123)

-- bar :: (Bar -> b) -> b
-- bar f = f . Bar $ Just 123

bar1 :: Bar
bar1 = Bar $ Just (123 :: Int)

runFoo :: IO ()
runFoo = foo \x -> print x

runBar :: Bar -> IO ()
runBar (Bar x) = print x

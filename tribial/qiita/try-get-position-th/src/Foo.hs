{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo where

import Language.Haskell.TH

foo :: [String] -> IO ()
foo [] = error $
	"\n" ++ $(litE =<< stringL . pprint <$> location) ++
	"\nfoo: The argument shoud not be empty."
foo strs = putStrLn `mapM_` strs

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module B where

import A (Test(test))

instance Test String Int where
	test s i = concat $ replicate i s

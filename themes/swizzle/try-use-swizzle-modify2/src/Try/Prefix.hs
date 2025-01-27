{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Try.Prefix where

import Data.SwizzleModify.TH

swizzleModify "modify" "wvtrq"

foo :: (Int, Int, Int, Int, Int, Int, String, Int, String, Char)
foo = modifyWvtrq ((* 100), (* 200), show, (`replicate` 'c'), const 'q')
	(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	-- (0, 1, 2, 300, 800, 5, "6", 7, "cccccccc", 'q')

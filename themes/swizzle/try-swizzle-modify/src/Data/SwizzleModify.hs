{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}

module Data.SwizzleModify where

import Data.SwizzleModify.TH

concat <$> (swizzleModify . (: "")) `mapM` ("xyz" ++ reverse ['a' .. 'w'])

concat <$> swizzleModify `mapM` (filter ((> 1) . length) [
	concat [x', y', z', w', v'] |
	x' <- ["", "x"], y' <- ["", "y"],
	z' <- ["", "z"], w' <- ["", "w"], v' <- ["", "v"] ])

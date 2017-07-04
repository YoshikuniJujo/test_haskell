-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

import Data.String

type FilePath' = forall s . IsString s => s

-- data FP = forall s . IsString s => FP s

-- type FilePath' = IsString s => s

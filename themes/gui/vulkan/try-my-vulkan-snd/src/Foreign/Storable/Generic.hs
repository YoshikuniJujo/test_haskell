{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module Foreign.Storable.Generic (
	module Foreign.Storable.Generic.Internal ) where

import Foreign.Storable.Generic.TH
import Foreign.Storable.Generic.Internal

instanceTuples `mapM` [2 .. 10]

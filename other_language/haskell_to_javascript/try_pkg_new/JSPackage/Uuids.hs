module JSPackage.Uuids where

import Data.Word

data UUID = UUID Word64 Word64 deriving Show

underTheControl :: UUID
underTheControl = UUID 0 0

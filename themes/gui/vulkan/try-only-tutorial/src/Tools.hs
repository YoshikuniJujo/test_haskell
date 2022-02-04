{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String
import Control.Monad.Cont

cStringListToCStringArray :: [CString] -> ContT r IO (Ptr CString)
cStringListToCStringArray cstrs = do
	pcstrs <- ContT $ allocaArray cstrc
	lift $ pokeArray pcstrs cstrs
	pure pcstrs
	where cstrc = length cstrs

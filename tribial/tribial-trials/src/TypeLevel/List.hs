{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevel.List where

import Foreign.Ptr

type family Fun as where
	Fun '[] = IO ()
	Fun (a ': as) = a -> Fun as

foo :: Fun '[Int, Char, Bool]
foo i c b = print i >> print c >> print b

-- foreign import ccall "wrapper" wrapFun ::
--	Fun as -> IO (FunPtr (Fun as))

type Fun1 a = a -> IO ()

foreign import ccall "wrapper" wrapFun1 ::
	Fun1 Int -> IO (FunPtr (Fun1 Int))

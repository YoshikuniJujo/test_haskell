{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

#include "foo.h"
#include <sys/socket.h>

#include "CVect.h"

-- import Lib

main :: IO ()
main = do
	putStrLn "Slozsoft"
	print (#{const ONE} :: Int)
	print (#{const ONE} :: Int)
	print @Int #const SOL_SOCKET
	print @Int #const SO_PEERCRED

data CVect

peekX :: Ptr CVect -> IO CInt
peekX = #{peek CVect, x}

peekY :: Ptr CVect -> IO CInt
peekY = #{peek CVect, y}

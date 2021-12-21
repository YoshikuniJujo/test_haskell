{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.Event where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Bool
import Data.Word
import System.IO
import System.IO.Unsafe

#include <human.h>

data Event s = Event (Ptr (Event s)) deriving Show

foreign import ccall "hm_get_event_only_tick"
	c_hm_get_event_only_tick :: IO (Ptr (Event s))

foreign import ccall "hm_event_destroy"
	c_hm_event_destroy :: Ptr (Event s) -> IO ()

withEventOnlyTick :: (forall s . Event s -> IO a) -> IO a
withEventOnlyTick f =
	bracket c_hm_get_event_only_tick c_hm_event_destroy (f . Event)

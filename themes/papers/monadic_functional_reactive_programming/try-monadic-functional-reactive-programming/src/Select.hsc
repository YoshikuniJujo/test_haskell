{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#include <sys/select.h>

module Select (select, fdClr) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Data.Time
import System.Posix.Types

data {-# CTYPE "sys/select.h" "fd_set" #-} FdSet = FdSet (Ptr FdSet)
data {-# CTYPE "sys/select.h" "struct timeval" #-} TimeVal = TimeVal NominalDiffTime

foreign import capi "sys/select.h FD_CLR" c_FD_CLR :: CInt -> Ptr FdSet -> IO ()
foreign import capi "sys/select.h FD_ISSET" c_FD_ISSET ::
	CInt -> Ptr FdSet -> IO Bool
foreign import capi "sys/select.h FD_SET" c_FD_SET :: CInt -> Ptr FdSet -> IO ()
foreign import capi "sys/select.h FD_ZERO" c_FD_ZERO :: Ptr FdSet -> IO ()

foreign import ccall "sys/select.h select" c_select ::
	Fd -> Ptr FdSet -> Ptr FdSet -> Ptr FdSet -> Ptr TimeVal -> IO CInt

instance Storable TimeVal where
	sizeOf _ = #size struct timeval
	alignment _ = #alignment struct timeval
	peek p = do
		s <- #{peek struct timeval, tv_sec} p
		us <- #{peek struct timeval, tv_usec} p
		pure . TimeVal $ fromIntegral (s :: CLong) +
			fromIntegral (us :: CLong) / 1000000
	poke p (TimeVal tv) = do
		#{poke struct timeval, tv_sec} p (s :: CLong)
		#{poke struct timeval, tv_usec} p (us :: CLong)
		where
		(s, us) = (round . (* 1000000)) `second` properFraction tv

fdClr :: Fd -> FdSet -> IO ()
fdClr (Fd fd) (FdSet fds) = c_FD_CLR fd fds

fdIsSet :: Fd -> FdSet -> IO Bool
fdIsSet (Fd fd) (FdSet fds) = c_FD_ISSET fd fds

fdSet :: Fd -> FdSet -> IO ()
fdSet (Fd fd) (FdSet fds) = c_FD_SET fd fds

fdZero :: FdSet -> IO ()
fdZero (FdSet fds) = c_FD_ZERO fds

select :: [Fd] -> [Fd] -> [Fd] -> NominalDiffTime -> IO [Fd]
select rds wts exs tv = alloca \ptv ->
	allocaBytes #{size fd_set} \rfds ->
	allocaBytes #{size fd_set} \wfds ->
	allocaBytes #{size fd_set} \efds -> do
	poke ptv $ TimeVal tv
	fdZero $ FdSet rfds
	fdZero $ FdSet wfds
	fdZero $ FdSet efds
	(`fdSet` FdSet rfds) `mapM_` rds
	(`fdSet` FdSet wfds) `mapM_` wts
	(`fdSet` FdSet efds) `mapM_` exs
	_ <- c_select (maximum (rds ++ wts ++ exs) + 1) rfds wfds efds ptv
	rr <- (`fdIsSet` FdSet rfds) `filterM` rds
	rw <- (`fdIsSet` FdSet wfds) `filterM` wts
	re <- (`fdIsSet` FdSet efds) `filterM` exs
	pure $ rr ++ rw ++ re

{-
newtype {-# CTYPE "sys/select.h" "sigset_t" #-} SigSet = SigSet (Ptr SigSet)

foreign import ccall "sys/select.h sigemptyset" c_sigemptyset :: Ptr SigSet -> IO ()
-}

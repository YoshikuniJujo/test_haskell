module Test where

import System.IO
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import qualified Data.ByteString as BS

import Tar
import Tools

ropen :: FilePath -> IO Handle
ropen = (`openFile` ReadMode)

alc :: (Ptr a -> IO b) -> IO b
alc = allocaBytes 512

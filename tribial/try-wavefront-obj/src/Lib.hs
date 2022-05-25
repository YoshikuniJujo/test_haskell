module Lib where

import Codec.Wavefront
import Codec.Wavefront.IO

import qualified Data.ByteString as BS

getSample :: IO WavefrontOBJ
getSample = either error pure =<< fromFile "../../files/models/viking_room.obj"

getTiny :: IO WavefrontOBJ
getTiny = either error pure =<< fromFile "tiny.obj"

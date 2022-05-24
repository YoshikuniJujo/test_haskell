module Lib where

import Codec.Wavefront
import Codec.Wavefront.IO

getSample :: IO WavefrontOBJ
getSample = either error pure =<< fromFile "../../files/models/viking_room.obj"

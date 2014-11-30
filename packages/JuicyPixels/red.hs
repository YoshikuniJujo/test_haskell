import Data.Word8
import Codec.Picture

red :: DynamicImage
red = ImageRGB8 $ generateImage (\x y -> PixelRGB8
		(adjust (800 - x) 800 (adjust y 300 maxBound))
		(maxBound - adjust (800 - x) 800 maxBound)
		(adjust (800 - x) 800 (maxBound - adjust y 300 maxBound))
	) 800 300

adjust :: Int -> Int -> Word8 -> Word8
adjust r w n = round (fromIntegral n * fromIntegral r / fromIntegral w :: Double)

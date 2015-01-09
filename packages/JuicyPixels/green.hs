import Codec.Picture

main :: IO ()
main = writePng "green.png" green

green :: Image PixelRGB8
green = generateImage (\_x _y -> PixelRGB8 0 maxBound 0) 16 16

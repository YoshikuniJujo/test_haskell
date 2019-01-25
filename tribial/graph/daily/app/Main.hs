import Control.Applicative
import Control.Arrow
import Data.Time
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import System.Environment

import Text.Read

dbase, vbase :: Double
dbase = 80
vbase = 320

dscale, vscale :: Double
dscale = 380
vscale = 300

posd :: (Day, Day) -> Day -> Double
posd (dmn, dmx) d = dbase +
	dscale * fromIntegral (diffDays d dmn) / fromIntegral (diffDays dmx dmn)

posv :: (Integer, Integer) -> Integer -> Double
posv (vmn, vmx) v = vbase +
	vscale * fromIntegral (vmn - v) / fromIntegral (vmx - vmn)

getDay :: (Day, Day) -> Double -> Day
getDay (dmn, dmx) x = (`addDays` dmn) . floor $
	(x - dbase) * fromIntegral (diffDays dmx dmn) / dscale

getValue :: (Integer, Integer) -> Double -> Integer
getValue (vmn, vmx) y = (vmn -) . floor $
	(y - vbase) * (fromIntegral $ vmx - vmn) / vscale

readData1 :: String -> (Day, Integer)
readData1 s = let (ds : vs : _) = words s in (readErr "foo" ds, readErr "bar" vs)

graph :: Turtle -> (Day, Day) -> (Integer, Integer) -> [(Day, Integer)] -> IO ()
graph t dsc vsc ((d, v) : dvs) = do
	penup t
	goto t (posd dsc d) (posv vsc v)
	pendown t
	mapM_ (uncurry (goto t) . (posd dsc *** posv vsc)) dvs
	penup t

readErr :: Read a => String -> String -> a
readErr em s = maybe (error em) id $ readMaybe s

main :: IO ()
main = do
	dmns : dmxs : vmns : vmxs : fp : _ <- getArgs
	let	dmn = readErr "0" dmns
		dmx = readErr "1" dmxs
		vmn = readErr "2" vmns
		vmx = readErr "3" vmxs
		pd = posd (dmn, dmx)
		pv = posv (vmn, vmx)
	dvs <- map readData1 . lines <$> readFile fp
	f <- openField
	onkeypress f (return . (/= 'q'))
	onclick f $ \_ x y -> do
		print (getDay (dmn, dmx) x, getValue (vmn, vmx) y)
		return True
	topleft f
	t <- newTurtle f
	penup t
	goto t (pd dmn) (pv vmx)
	pendown t
	goto t (pd dmn) (pv vmn)
	goto t (pd dmx) (pv vmn)
	penup t
	goto t (pd dmn - 35) (pv vmn + 25)
	write t "KochiGothic" 12 $ show dmn
	goto t (pd dmx - 35) (pv vmn + 25)
	write t "KochiGothic" 12 $ show dmx
	goto t (pd dmn - 70) (pv vmn)
	write t "KochiGothic" 12 $ show vmn
	goto t (pd dmn - 70) (pv vmx + 10)
	write t "KochiGothic" 12 $ show vmx
	graph t (dmn, dmx) (vmn, vmx) $ dropWhile ((<= addDays 10 dmn) . fst) dvs
	hideturtle t
	svg <- getSVG t
	putStrLn $ showSVG 750 500 $ map ((,) "foo") svg
	waitField f

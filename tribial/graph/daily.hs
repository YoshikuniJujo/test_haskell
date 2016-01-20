import Data.Time
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import System.Environment

dbase, vbase :: Double
dbase = 20
vbase = 200

dscale, vscale :: Double
dscale = 20
vscale = 1

posd :: (Day, Day) -> Day -> Double
posd (dmn, dmx) d =
	dbase + fromIntegral (diffDays d dmn) / fromIntegral (diffDays dmx dmn)

posv :: Integral n => (n, n) -> n -> Double
posv (vmn, vmx) v = vbase + fromIntegral (v - vmn) / fromIntegral (vmx - vmn)

main :: IO ()
main = do
	dmns : dmxs : vmns : vmxs : fp : _ <- getArgs
	f <- openField
	onkeypress f (return . (/= 'q'))
	topleft f
	t <- newTurtle f
	penup t
--	goto t (posd
	waitField f

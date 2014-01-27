import Prelude hiding (id, (.))

import Control.Category
import Control.Arrow

newtype IOMcn b c = IOMcn { runIOMcn :: b -> IO c }

instance Category IOMcn where
	id = IOMcn return
	IOMcn io1 . IOMcn io2 = IOMcn $ (io1 =<<) . io2

instance Arrow IOMcn where
	arr = IOMcn . (return .)

putStrLnM :: IOMcn String ()
putStrLnM = IOMcn putStrLn

getLineM :: IOMcn () String
getLineM = IOMcn $ const getLine

appM :: IOMcn (IOMcn b c, b) c
appM = IOMcn $ \(IOMcn io, x) -> io x

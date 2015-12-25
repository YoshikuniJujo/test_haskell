{-# LANGUAGE PackageImports #-}

import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import "monads-tf" Control.Monad.State (StateT, runStateT, gets, put)
import Data.Bits ((.|.), shiftL)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import System.IO (openBinaryFile, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)

import YjChunks (YjChunks, YjChunk, yjChunk, magic, dend)
import CRC (check)

decode :: BS.ByteString -> Maybe (YjChunks, BS.ByteString)
decode = runStateT $
	item 8 (bool Nothing (Just ()) . (== magic)) >> untilM (== dend) chunk

chunk :: StateT BS.ByteString Maybe YjChunk
chunk = item 4 (Just . le) >>= \l -> item (l + 8) $ \tdc ->
	if not $ check tdc then Nothing else
		let (t, dc) = BS.splitAt 4 tdc in yjChunk t $ BS.take l dc
	where le = (. BS.uncons) $
		maybe 0 (uncurry (.|.) . (fromIntegral *** (`shiftL` 8) . le))

untilM :: (Monad m, Functor m) => (a -> Bool) -> m a -> m [a]
untilM p m = bool (return []) <$> ((<$> untilM p m) . (:)) <*> not . p =<< m

item :: Int -> (BS.ByteString -> Maybe a) -> StateT BS.ByteString Maybe a
item l f = gets (BS.splitAt l) >>=
	uncurry (flip . maybe $ fail "error") . (f *** (. return) . (>>) . put)

sample :: BS.ByteString
sample = unsafePerformIO $
	BS.hGetContents =<< openBinaryFile "hello.yjcs" ReadMode

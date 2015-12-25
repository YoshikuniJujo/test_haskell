{-# LANGUAGE PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import Data.Bool
import qualified Data.ByteString as BS
import System.IO
import System.IO.Unsafe

import YjChunks
import CRC2

type ByteStringM = StateT BS.ByteString Maybe

decode :: BS.ByteString -> Maybe (YjChunks, BS.ByteString)
decode = runStateT $
	item 8 (bool Nothing (Just ()) . (== magic)) >> untilM (== dend) chunk

item :: Int -> (BS.ByteString -> Maybe a) -> ByteStringM a
item l f = gets (BS.splitAt l) >>= \(bs, r) ->
	maybe (fail "error") ((put r >>) . return) $ f bs

chunk :: ByteStringM YjChunk
chunk = item 4 (Just . toNum) >>= \l -> item (l + 8) $ \tdc ->
	if not $ check tdc then Nothing else
		let (t, dc) = BS.splitAt 4 tdc in mkYjChunk t $ BS.take l dc

untilM :: (Monad m, Functor m) => (a -> Bool) -> m a -> m [a]
untilM p m = flip bool (return []) <$> ((<$> untilM p m) . (:)) <*> p =<< m

sample :: BS.ByteString
sample = unsafePerformIO $
	BS.hGetContents =<< openBinaryFile "hello.yjcs" ReadMode

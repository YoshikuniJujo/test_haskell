import Prelude hiding (head, tail)
import Control.Applicative

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

data LList a = LList (Ptr a) (Ptr (LList a)) deriving Show

instance Storable a => Storable (LList a) where
	sizeOf _ = sizeOf (undefined :: Ptr ()) + sizeOf (undefined :: Ptr ())
	alignment _ = alignment (undefined :: Ptr ()) + alignment (undefined :: Ptr ())
	poke ptr (LList x nl) = poke (castPtr ptr) x >> pokeByteOff ptr (sizeOf x) nl
	peek ptr = do
		x <- peek (castPtr ptr)
		nl <- peekByteOff ptr (sizeOf x)
		return $ LList x nl

single :: Storable a => a -> IO (LList a)
single x = do
	ptr <- malloc
	poke ptr x
	return $ LList ptr nullPtr

cons :: Storable a => a -> LList a -> IO (LList a)
cons x lst = do
	xptr <- malloc
	poke xptr x
	ptr <- malloc
	poke ptr lst
	return $ LList xptr ptr

head :: Storable a => LList a -> IO a
head (LList xptr _) = peek xptr

tail :: Storable a => LList a -> IO (LList a)
tail (LList _ ptr) = peek ptr

fromList :: Storable a => [a] -> IO (LList a)
fromList [x] = single x
fromList (x : xs) = cons x =<< fromList xs

toList :: Storable a => LList a -> IO [a]
toList lst@(LList xptr ptr)
	| ptr == nullPtr = (: []) <$> peek xptr
	| otherwise = (:) <$> head lst <*> (toList =<< tail lst)

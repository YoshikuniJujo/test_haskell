{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH
-- import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe

newtype Bar = Bar Int deriving Show

instanceStorable0 :: IO [Dec]
instanceStorable0 = runQ [d|
	instance Storable Bar where
		sizeOf _ = sizeOf (undefined :: Int)
		alignment _ = alignment (undefined :: Int)
		peek p = Bar <$> peek p
		poke p (Bar x) = poke p x
	|]

funs0 :: [Dec]
[InstanceD _ _ _ funs0] = unsafePerformIO instanceStorable0

deriveStorable0 :: Name -> Name -> DecQ
deriveStorable0 drv org = newName `mapM` ["p", "p", "x"] >>= \[p, p', x] ->
	instanceD (cxt []) (appT (conT ''Storable) (conT drv)) [
		funD 'sizeOf [clause [wildP]
			(normalB $ varE 'sizeOf `appE`
				(varE 'undefined `sigE` conT org))
			[]],
		funD 'alignment [clause [wildP]
			(normalB $ varE 'alignment `appE`
				(varE 'undefined `sigE` conT org))
			[]],
		funD 'peek [clause [varP p]
			(normalB $ infixE (Just $ conE drv) (varE '(<$>))
				. Just $ varE 'peek `appE` varE p)
			[]],
		funD 'poke [clause [varP p', conP drv [varP x]]
			(normalB $ varE 'poke `appE` varE p `appE` varE x)
			[]] ]

mkFuns0 :: [Dec]
InstanceD _ _ _ mkFuns0 = unsafePerformIO . runQ $ deriveStorable0 ''Bar ''Int

data Foo = Foo_ (ForeignPtr Foo)

instanceStorable :: IO [Dec]
instanceStorable = runQ [d|
	instance Storable Foo where
		sizeOf _ = 123
		alignment _ = 456
		peek ps = do
			pd <- malloc
			copyBytes pd ps 123
			Foo_ <$> newForeignPtr pd (free pd)
		poke pd (Foo_ fps) =
			withForeignPtr fps \ps -> copyBytes pd ps 123
	|]

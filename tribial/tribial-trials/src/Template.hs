{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template where

import Language.Haskell.TH
-- import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
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

data FooBar = FooBar_ (ForeignPtr FooBar)

instanceStorable :: IO [Dec]
instanceStorable = runQ [d|
	instance Storable FooBar where
		sizeOf _ = 123
		alignment _ = 456
		peek ps = do
			pd <- malloc
			copyBytes pd ps 123
			FooBar_ <$> newForeignPtr pd (free pd)
		poke pd (FooBar_ fps) =
			withForeignPtr fps \ps -> copyBytes pd ps 123
	|]

funs :: [Dec]
[InstanceD _ _ _ funs] = unsafePerformIO instanceStorable

deriveStorable :: String -> Integer -> Integer -> DecQ
deriveStorable n sz algn = do
	[ps, pd, pd', fps', ps'] <-
		newName `mapM` ["ps", "pd", "pd", "fps", "ps"]
	instanceD (cxt []) (appT (conT ''Storable) (conT tp)) [
		funD 'sizeOf [clause [wildP] (normalB . litE $ integerL sz) []],
		funD 'alignment
			[clause [wildP] (normalB . litE $ integerL algn) []],
		funD 'peek [clause [varP ps] (normalB $ doE [
			bindS (varP pd) $ varE 'malloc,
			noBindS $ varE 'copyBytes `appE` varE pd `appE`
				varE ps `appE` litE (integerL sz),
			noBindS . infixE (Just $ conE dc) (varE '(<$>))
				. Just $ varE 'newForeignPtr
					`appE` varE pd
					`appE` (varE 'free `appE` varE pd)
			]) [] ],
		funD 'poke [clause [varP pd', conP dc [varP fps']] (normalB
			$ varE 'withForeignPtr
				`appE` varE fps' `appE` (lamE [varP ps']
					$ varE 'copyBytes `appE` varE pd'
						`appE` varE ps'
						`appE` litE (integerL sz))) []]
		]
	where tp = mkName n; dc = mkName $ n ++ "_"

drFuns :: [Dec]
InstanceD _ _ _ drFuns = unsafePerformIO . runQ $ deriveStorable "FooBar" 123 456

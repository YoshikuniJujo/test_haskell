{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.SwizzleLens.TH (swizzleLens) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.List qualified as L
import Data.Char

import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS
import Data.SwizzleLens.Pkgs

someFunc :: IO ()
someFunc = putStrLn "someFunc"

x :: (Swz.Swizzle1 a, SwzS.SwizzleSet1 a, Swz.X a ~ SwzS.X a, Functor f) =>
	(Swz.X a -> f (Swz.X a)) -> a -> f a
x f a = flip SwzS.x a <$> f (Swz.x a)

xy :: (Swz.Swizzle2 a, SwzS.SwizzleSet2 a,
	Swz.X a ~ SwzS.X a, Swz.Y a ~ SwzS.Y a, Functor f) =>
	((Swz.X a, Swz.Y a) -> f (Swz.X a, Swz.Y a)) -> a -> f a
xy f a = s <$> f (Swz.x a, Swz.y a)
	where
	s (x', y') = SwzS.x x' . SwzS.y y' $ a

newtype Const c a = Const { runConst :: c } deriving Show

instance Functor (Const c) where _ `fmap` Const c = Const c

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where f `fmap` Identity x = Identity $ f x

swizzleLens :: String -> DecsQ
swizzleLens nm = sequence [tdec nm, fun nm]

tdec0 :: DecsQ
tdec0 = [d|
	xy :: (Swz.Swizzle2 a, SwzS.SwizzleSet2 a,
		Swz.X a ~ SwzS.X a, Swz.Y a ~ SwzS.Y a, Functor f) =>
		((Swz.X a, Swz.Y a) -> f (Swz.X a, Swz.Y a)) -> a -> f a
	xy = undefined
	|]

tdec :: String -> DecQ
tdec nm = newName "a" >>= \a -> newName "f" >>= \f -> sigD (mkName nm) .
	forallT [] (cxt $ [
		swzSwizzleN n `appT` varT a,
		swzsSwizzleSetN n `appT` varT a ] ++ ((`teq` a) <$> nm) ++ [
		conT ''Functor `appT` varT f  ]) $
		(tup a `arrT` varT f `appT` tup a)
		`arrT` varT a `arrT` varT f `appT` varT a
	where
	n = maxN nm
	teq c a = swzXT c `appT` varT a `eqT` swzsXT c `appT` varT a
	tup a = tupT $ (`appT` varT a) . swzXT <$> nm

fun0 :: DecsQ
fun0 = [d|
	xy f a = s <$> f (Swz.x a, Swz.y a)
		where
		s (x', y') = SwzS.x x' . SwzS.y y' $ a
	|]

fun :: String -> DecQ
fun nm = newName "f" >>= \f -> newName "a" >>= \a -> newName "s" >>= \s ->
	(newName . (: "")) `mapM` nm >>= \xy ->
	funD (mkName nm) [
		clause [varP f, varP a] (normalB $
			varE s `fmapE` varE f `appE` (tupE' $ (`appE` varE a) . swzXF <$> nm)
			) [
			funD s [
				clause [tupP' $ varP <$> xy] (normalB $
					(	foldr1 comE $ zipWith ((. varE) . appE . swzsXF) nm xy
						)
					`appE` varE a
					) []
				]] ]

swzSwizzleN :: Int -> TypeQ
swzSwizzleN n =
	conT . mkNameG_tc swizzlePkg "Data.Swizzle.Class.Base" . ("Swizzle" ++) $ show n

swzsSwizzleSetN :: Int -> TypeQ
swzsSwizzleSetN n =
	conT . mkNameG_tc swizzleSetPkg "Data.SwizzleSet.Class.Base" . ("SwizzleSet" ++) $ show n

swzXT :: Char -> TypeQ
swzXT = conT . mkNameG_tc swizzlePkg "Data.Swizzle.Class.Base" . (: "") . toUpper

swzsXT :: Char -> TypeQ
swzsXT = conT . mkNameG_tc swizzleSetPkg "Data.SwizzleSet.Class.Base" . (: "") . toUpper

swzXF :: Char -> ExpQ
swzXF = varE . mkNameG_v swizzlePkg "Data.Swizzle.Class.Base" . (: "")

swzsXF :: Char -> ExpQ
swzsXF = varE . mkNameG_v swizzleSetPkg "Data.SwizzleSet.Class.Base" . (: "")

infixr 6 `arrT`

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT ` t2 = arrowT `appT` t1 `appT` t2

tupT :: [TypeQ] -> TypeQ
tupT = \case [t] -> t; ts -> foldl appT (tupleT $ length ts) ts

tupP' :: [PatQ] -> PatQ
tupP' = \case [p] -> p; ps -> tupP ps

tupE' :: [ExpQ] -> ExpQ
tupE' = \case [e] -> e; es -> tupE es

infixr 6 `eqT`

eqT :: TypeQ -> TypeQ -> TypeQ
t1 `eqT` t2 = equalityT `appT` t1 `appT` t2

infixr 6 `fmapE`

fmapE :: ExpQ -> ExpQ -> ExpQ
e1 `fmapE` e2 = infixE (Just e1) (varE '(<$>)) (Just e2)

infixr 7 `comE`

comE :: ExpQ -> ExpQ -> ExpQ
e1 `comE` e2 = infixE (Just e1) (varE '(.)) (Just e2)

alphabet :: [Char]
alphabet = "xyz" ++ reverse ['a' .. 'w']

alphToN :: Char -> Maybe Int
alphToN c = (+ 1) <$> c `L.elemIndex` alphabet

maxN :: [Char] -> Int
maxN = maximum . catMaybes . (alphToN <$>)

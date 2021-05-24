{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template (
	-- * STRUCT
	struct, StrName, StrSize,
	MemName, MemType, MemPeek, MemPoke, DerivClass,
	-- * STRUCT WITH PRIMITIVE MONAD
	structPrim) where

import Language.Haskell.TH (
	DecsQ, DecQ, Dec(PragmaD), Pragma(CompleteP), sigD, valD, funD, tySynD,
	newtypeD, plainTV, normalC, derivClause,
		bangType, bang, noSourceUnpackedness, noSourceStrictness,
	instanceD, cxt,
	patSynSigD, patSynD, recordPatSyn, explBidir,
	ExpQ, varE, conE, appE, lamE, tupE, listE,
	forallT, varT, conT, appT, varP, wildP, conP, tupP, viewP,
	Name, mkName, newName,
	ClauseQ, clause, normalB, StmtQ, doE, compE, bindS, noBindS )
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal (mallocBytes, free)
import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Control.Monad.Primitive (PrimMonad(..), RealWorld, unsafeIOToPrim)
import Data.Bool (bool)
import Data.Maybe (mapMaybe)
import Data.List (unzip4, intersperse, intercalate)
import Data.Array (Ix(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Lexeme(..), readPrec, step, lexP, parens, prec)

import Template.Parts (
	(.->), (.$), (...), (.<$>), (.<*>), (.>>=),
	(.&&), (.||), (.==), (.<), (.+), (.*),
	tupleE, tupT, tupP', litI, strP, pt, zp, ss, (..+), toLabel, lcfirst )

---------------------------------------------------------------------------

-- * STRUCT
--	+ FUNCTION STRUCT
--	+ NEWTYPE
--	+ PATTERN
--		- Function Mk Pattern
--		- Function Mk Pattern Fun
--	+ DERIVING
--		- Function Mk Deriving
--		- Show
--		- Read
--		- Eq
--		- Ord
--		- Bounded
--		- Ix
-- * STRUCT WITH PRIMITIVE MONAD
-- 	+ FUNCTION STRUCT PRIM
-- 	+ NEWTYPE
-- 	+ FREEZE
-- 	+ THAW
-- 	+ COPY

---------------------------------------------------------------------------
-- STRUCT
---------------------------------------------------------------------------

-- FUNCTION STRUCT

struct :: StrName -> StrSize ->
	[(MemName, MemType, MemPeek, MemPoke)] -> [DerivClass] -> DecsQ
struct sn sz (unzip4 -> (mns, mts, mpes, mpos)) dcs_ = (++)
	<$> sequence [
		mkNewtype sn,
		pure . PragmaD $ CompleteP [mkName sn] Nothing,
		mkPatternSig sn mts, mkPatternBody sn sz mns mpos,
		mkPatternFunSig sn mts, mkPatternFunBody sn mpes ]
	<*> mkInstances sn mns dcs
	where dcs = case toDerivCollection dcs_ of
		(d, []) -> d; (_, os) -> error $ "Can't derive: " ++ show os

type StrName = String; type StrSize = Integer
type MemName = String; type MemType = Name
type MemPeek = ExpQ; type MemPoke = ExpQ
type DerivClass = Name

-- NEWTYPE

mkNewtype :: StrName -> DecQ
mkNewtype sn =
	newtypeD (cxt []) (mkName sn) [] Nothing (normalC (mkName $ sn ++ "_") [
		bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''ForeignPtr `appT` conT (mkName sn)) ]) []

-- PATTERN

-- Function Mk Pattern

mkPatternSig :: StrName -> [MemType] -> DecQ
mkPatternSig (mkName -> sn) = patSynSigD sn . foldr (.->) (conT sn) . (conT <$>)

mkPatternBody :: StrName -> StrSize -> [MemName] -> [MemPoke] -> DecQ
mkPatternBody sn sz ms_ pos = patSynD (mkName sn) (recordPatSyn ms)
	(explBidir [mkPatternBodyClause sn sz pos])
	(viewP (varE . mkName $ lcfirst sn) (tupP' $ varP <$> ms))
	where ms = mkName . toLabel sn <$> ms_

mkPatternBodyClause :: StrName -> StrSize -> [MemPoke] -> ClauseQ
mkPatternBodyClause (mkName . (++ "_") -> sn) sz pos = do
	(vs, p) <- (,) <$> length pos `replicateM` newName "v" <*> newName "p"
	let	vps = varP <$> vs; pe = varE p; fr = varE 'free `appE` pe
	clause vps (normalB $ varE 'unsafePerformIO .$ conE sn .<$> doE (
		(varP p `bindS` (varE 'mallocBytes `appE` litI sz)) :
		((<$> zip pos vs) \(po, v) ->
			noBindS $ po `appE` pe `appE` varE v) ++
		[noBindS $ varE 'newForeignPtr `appE` pe `appE` fr] )) []

-- Function Mk Pattern Fun

mkPatternFunSig :: StrName -> [MemType] -> DecQ
mkPatternFunSig (mkName . lcfirst &&& conT . mkName -> (fn, st)) =
	sigD fn . (st .->) . tupT . (conT <$>)

mkPatternFunBody :: StrName -> [MemPeek] -> DecQ
mkPatternFunBody (mkName . lcfirst &&& mkName . (++ "_") -> (fn, cn)) pes =
	funD fn . (: []) $ (,) <$> newName "f" <*> newName "p" >>= \(f, p) ->
		clause [conP cn [varP f]] (normalB $ varE 'unsafePerformIO
			.$ varE 'withForeignPtr `appE` varE f
				`appE` lamE [bool (varP p) wildP $ null pes]
					(mkPatternFunPeeks p pes)) []

mkPatternFunPeeks :: Name -> [MemPeek] -> ExpQ
mkPatternFunPeeks (varE -> p) (length &&& id -> (n, pes)) =
	foldl (.<*>) (varE 'pure .$ tupleE n) $ (`appE` p) <$> pes

-- DERIVING

-- Function Mk Deriving

mkInstances :: StrName -> [MemName] -> DerivCollection -> DecsQ
mkInstances sn ms dc =
	sequence $ (\(t, b) -> bool Nothing (Just t) b) `mapMaybe` zip [
		mkInstanceShow sn ms, mkInstanceRead sn ms, mkInstanceEq sn ms,
		mkInstanceOrd sn ms, mkInstanceBounded sn ms, mkInstanceIx sn ms
		] [	derivingShow dc, derivingRead dc, derivingEq dc,
			derivingOrd dc, derivingBounded dc, derivingIx dc ]

data DerivCollection = DerivCollection {
	derivingShow :: Bool, derivingRead :: Bool,
	derivingEq :: Bool, derivingOrd :: Bool,
	derivingBounded :: Bool, derivingIx :: Bool } deriving Show

toDerivCollection :: [DerivClass] -> (DerivCollection, [DerivClass])
toDerivCollection [] = (DerivCollection False False False False False False, [])
toDerivCollection (d : ds) = case d of
	NameShow -> (dc { derivingShow = True }, ds')
	NameRead -> (dc { derivingRead = True }, ds')
	NameEq -> (dc { derivingEq = True }, ds')
	NameOrd -> (dc { derivingOrd = True }, ds')
	NameBounded -> (dc { derivingBounded = True }, ds')
	NameIx -> (dc {derivingIx = True }, ds')
	_ -> (dc, d : ds')
	where (dc, ds') = toDerivCollection ds

pattern NameShow, NameRead, NameEq, NameOrd, NameBounded, NameIx :: Name
pattern NameShow <- ((== ''Show) -> True)
pattern NameRead <- ((== ''Read) -> True)
pattern NameEq <- ((== ''Eq) -> True)
pattern NameOrd <- ((== ''Ord) -> True)
pattern NameBounded <- ((== ''Bounded) -> True)
pattern NameIx <- ((== ''Ix) -> True)

-- Show

mkInstanceShow :: StrName -> [MemName] -> DecQ
mkInstanceShow (mkName &&& id -> (sn, ssn)) ms = do
	(s, vs) <- (,) <$> newName "s" <*> length ms `replicateM` newName "v"
	instanceD (cxt []) (conT ''Show `appT` conT sn) . (: [])
		$ funD 'showsPrec [clause [wildP, varP s]
			(normalB $ ss (ssn ++ " {") ...
				mkShowMems ssn ms vs ... ss "}")
			[valD (conP sn $ varP <$> vs) (normalB $ varE s) []]]

mkShowMems :: StrName -> [MemName] -> [Name] -> ExpQ
mkShowMems (toLabel -> l) ms vs = foldr (...) (varE 'id) . intersperse (ss ", ")
	$ (<$> zip ms vs) \(m, v) ->
		l m ..+ " = " ... varE 'showsPrec `appE` litI 11 `appE` varE v

-- Read

mkInstanceRead :: StrName -> [MemName] -> DecQ
mkInstanceRead sn ms = length ms `replicateM` newName "v" >>= \vs ->
	instanceD (cxt []) (conT ''Read `appT` t) . (: [])
		$ valD (varP 'readPrec) (normalB $ varE 'parens
			.$ varE 'prec `appE` litI 10 `appE` doE ([
				conP 'Ident [strP sn] `bindS` varE 'lexP,
				conP 'Punc [strP "{"] `bindS` varE 'lexP ] ++
				mkReadMems sn ms vs ++ [
				conP 'Punc [strP "}"] `bindS` varE 'lexP,
				noBindS $ varE 'pure
					.$ foldl appE c (varE <$> vs) ])) []
	where t = conT $ mkName sn; c = conE $ mkName sn

mkReadMems :: StrName -> [MemName] -> [Name] -> [StmtQ]
mkReadMems sn ms vs =
	intercalate [conP 'Punc [strP ","] `bindS` varE 'lexP]
		$ (<$> zip ms vs) \(m, v) -> [
			conP 'Ident [strP $ toLabel sn m] `bindS` varE 'lexP,
			conP 'Punc [strP "="] `bindS` varE 'lexP,
			varP v `bindS` (varE 'step `appE` varE 'readPrec) ]

-- Eq

mkInstanceEq :: StrName -> [MemName] -> DecQ
mkInstanceEq sn ms = (,) <$> newName "s" <*> newName "t" >>= \(s, t) ->
	instanceD (cxt []) (conT ''Eq `appT` conT (mkName sn)) . (: [])
		. funD '(==) . (: []) $ clause [varP s, varP t] (normalB
			$ foldl (.&&) (conE 'True) $ mkMemEq sn s t <$> ms) []

mkMemEq :: StrName -> Name -> Name -> MemName -> ExpQ
mkMemEq sn (varE -> s) (varE -> t) m = let l = varE . mkName $ toLabel sn m in
	l `appE` s .== l `appE` t

-- Ord

mkInstanceOrd :: StrName -> [MemName] -> DecQ
mkInstanceOrd sn ms = (,) <$> newName "s" <*> newName "t" >>= \(s, t) ->
	instanceD (cxt []) (conT ''Ord `appT` conT (mkName sn)) . (: [])
		. funD '(<=) . (: []) $ clause [varP s, varP t] (
			normalB $ varE 'foldr `appE` lamOrd s t `appE`
				conE 'True `appE` listE ln ) []
	where ln = varE . mkName . toLabel sn <$> ms

lamOrd :: Name -> Name -> ExpQ
lamOrd (varE -> s) (varE -> t) =
	(,) <$> newName "x" <*> newName "v" >>= \(x, v) -> let xe = varE x in
		lamE [varP x, varP v] $ xe `appE` s .< xe `appE` t .||
			xe `appE` s .== xe `appE` t .&& varE v

-- Bounded

mkInstanceBounded :: StrName -> [MemName] -> DecQ
mkInstanceBounded (mkName -> sn) (length -> n) =
	instanceD (cxt []) (conT ''Bounded `appT` conT sn) [
		valD (varP 'minBound) (normalB $ foldl appE (conE sn)
			(replicate n $ varE 'minBound)) [],
		valD (varP 'maxBound) (normalB $ foldl appE (conE sn)
			(replicate n $ varE 'maxBound)) [] ]

-- Ix

mkInstanceIx :: StrName -> [MemName] -> DecQ
mkInstanceIx (mkName -> sn) ms = instanceD (cxt []) (conT ''Ix `appT` conT sn) [
	mkRange 'range sn ms, mkIndex 'index sn ms, mkInRange 'inRange sn ms ]

mkRange :: Name -> Name -> [MemName] -> DecQ
mkRange fn sn (length -> n) = do
	(vs, ws, is) <- unzip3 <$> n `replicateM`
		((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn . (: []) $ clause
		[tupP [conP sn $ varP <$> vs, conP sn $ varP <$> ws]]
		(normalB . compE . (++ [noBindS . foldl appE sne $ varE <$> is])
			$ (<$> is `zip` (vs `zip` ws)) \(i, (v, w)) ->
				bindS (varP i) $ rg `appE` tupE [varE v, varE w]
			) []
	where rg = varE 'range; sne = conE sn

mkIndex :: Name -> Name -> [MemName] -> DecQ
mkIndex fn (conP -> sn) (length -> n) = do
	(vs, ws, is) <- unzip3 <$> n `replicateM`
		((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn . (: []) $ clause
		[tupP [sn $ varP <$> vs, sn $ varP <$> ws], sn $ varP <$> is]
		(normalB $ varE 'foldl `appE` mkIndexLam `appE` litI 0
			.$ listE (varE <$> vs) `zp` listE (varE <$> ws) `zp`
				listE (varE <$> is)) []

mkIndexLam :: ExpQ
mkIndexLam =
	(,,) <$> newName "v" <*> newName "z" <*> newName "k" >>= \(v, z, k) ->
		lamE [varP v, tupP [varP z, varP k]]
			$ (varE 'index `appE` varE z `appE` varE k) .+
				(varE 'rangeSize `appE` varE z .* varE v)

mkInRange :: Name -> Name -> [MemName] -> DecQ
mkInRange fn (conP -> sn) (length -> n) = do
	(vs, ws, is) <- unzip3 <$> n `replicateM`
		((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn . (: []) $ clause
		[tupP [sn $ varP <$> vs, sn $ varP <$> ws], sn $ varP <$> is]
		(normalB . foldr (.&&) (conE 'True) $
			(<$> vs `zip` ws `zip` is) \((v, w), i) ->
				ir `appE` tupE [varE v, varE w] `appE` varE i
			) []
	where ir = varE 'inRange

---------------------------------------------------------------------------
-- STRUCT WITH PRIMITIVE MONAD
---------------------------------------------------------------------------

-- FUNCTION STRUCT PRIM

structPrim :: StrName -> FunCopy -> FunFree -> [DerivClass] -> DecsQ
structPrim nt cp fr ds = sequence [
	mkNewtypePrim nt ds, mkTypeST nt, mkTypeIO nt,
	mkFreezeSig nt, mkFreezeFun nt cp fr, mkThawSig nt, mkThawFun nt cp fr,
	mkCopySig nt, mkCopyFun nt cp fr ]

type FunCopy = Name; type FunFree = Name

-- NEWTYPE AND TYPE SYNONYM

mkNewtypePrim :: StrName -> [DerivClass] -> DecQ
mkNewtypePrim sn ds = newName "s" >>= \s ->
	newtypeD (cxt []) snp [plainTV s] Nothing
		(normalC snp . (: []) $ bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''ForeignPtr `appT` conT (mkName sn)))
		[derivClause Nothing $ conT <$> ds]
	where snp = mkName $ sn ++ "Prim"

mkTypeIO :: StrName -> DecQ
mkTypeIO sn = tySynD (mkName $ sn ++ "IO") []
	$ conT (mkName $ sn ++ "Prim") `appT` conT ''RealWorld

mkTypeST :: StrName -> DecQ
mkTypeST sn = tySynD (mkName $ sn ++ "ST") [] . conT . mkName $ sn ++ "Prim"

-- FREEZE

mkFreezeSig :: StrName -> DecQ
mkFreezeSig sn = newName "m" >>= \m ->
	sigD fn . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT snp `appT` (conT ''PrimState `appT` varT m) .->
			varT m `appT` conT (mkName sn)
	where fn = mkName $ lcfirst sn ++ "Freeze"; snp = mkName $ sn ++ "Prim"

mkFreezeFun :: StrName -> FunCopy -> FunFree -> DecQ
mkFreezeFun sn cp fr = newName "fp" >>= \fp ->
	funD (mkName $ lcfirst sn ++ "Freeze") . (: []) $
		clause [conP (mkName $ sn ++ "Prim") [varP fp]] (normalB
			$ mkFreezeBody sn cp fr fp) []

mkFreezeBody :: StrName -> FunCopy -> FunFree -> Name -> ExpQ
mkFreezeBody sn cp fr fp =
	varE 'unsafeIOToPrim ... conE (mkName $ sn ++ "_") `pt` varE '(<$>)
		.$ varE 'withForeignPtr `appE` varE fp `appE` varE cp
			.>>= varE 'newForeignPtr .<$> varE 'id .<*> varE fr

-- THAW

mkThawSig :: StrName -> DecQ
mkThawSig sn = newName "m" >>= \m ->
	sigD fn . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName sn) .-> varT m `appT`
			(conT snp `appT` (conT ''PrimState `appT` varT m))
	where fn = mkName $ lcfirst sn ++ "Thaw"; snp = mkName $ sn ++ "Prim"

mkThawFun :: StrName -> FunCopy -> FunFree -> DecQ
mkThawFun sn cp fr = newName "fp" >>= \fp ->
	funD (mkName $ lcfirst sn ++ "Thaw") . (: [])
		$ clause [conP (mkName $ sn ++ "_") [varP fp]] (
			normalB $ mkThawBody sn cp fr fp) []

mkThawBody :: StrName -> FunCopy -> FunFree -> Name -> ExpQ
mkThawBody sn cp fr fp =
	varE 'unsafeIOToPrim ... conE (mkName $ sn ++ "Prim") `pt` varE '(<$>)
		.$ varE 'withForeignPtr `appE` varE fp `appE` varE cp
			.>>= varE 'newForeignPtr .<$> varE 'id .<*> varE fr

-- COPY

mkCopySig :: String -> DecQ
mkCopySig nt = do
	m <- newName "m"
	sigD (mkName $ lcfirst nt ++ "Copy") . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m) .->
			(varT m `appT` (conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m)))

mkCopyFun :: String -> Name -> Name -> DecQ
mkCopyFun nt frz fr = do
	ff <- newName "ff"
	funD (mkName $ lcfirst nt ++ "Copy") [
		clause [conP (mkName $ nt ++ "Prim") [varP ff]] (
			normalB (mkCopyBody nt frz fr ff)
			) []
		]

mkCopyBody :: String -> Name -> Name -> Name -> ExpQ
mkCopyBody nt frz fr ff = (varE 'unsafeIOToPrim ... (conE (mkName $ nt ++ "Prim") `pt` varE '(<$>)))
	.$ (varE 'withForeignPtr `appE` varE ff `appE` varE frz) .>>= (varE 'newForeignPtr .<$> varE 'id .<*> varE fr)

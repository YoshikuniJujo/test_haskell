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
	ExpQ, varE, conE, litE, appE, lamE, tupE, listE,
	forallT, varT, conT, appT,
	varP, wildP, conP, litP, tupP, viewP, integerL, stringL,
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
	tupleE, tupT, tupP', litI, pt, zp, pp, lcfirst, ucfirst )

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
	where dcs = case toDeriving dcs_ of
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
	where ms = mkName . (lcfirst sn ++) . ucfirst <$> ms_

mkPatternBodyClause :: StrName -> StrSize -> [MemPoke] -> ClauseQ
mkPatternBodyClause (mkName . (++ "_") -> sn) sz pos = do
	(vs, p) <- (,) <$> length pos `replicateM` newName "v" <*> newName "p"
	let	vps = varP <$> vs; pe = varE p; fr = varE 'free `appE` pe
	clause vps (normalB $ varE 'unsafePerformIO .$ conE sn .<$> doE (
		bindS (varP p) (varE 'mallocBytes `appE` litI sz) :
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

mkInstances :: String -> [String] -> Deriving -> DecsQ
mkInstances nt fs d = sequence $ (\(t, b) -> bool Nothing (Just t) b) `mapMaybe` zip [
	mkInstanceShow nt fs, mkInstanceRead nt fs, mkInstanceEq nt fs,
	mkInstanceOrd nt fs, mkInstanceBounded nt fs, mkInstanceIx nt fs ] [
	derivingShow d, derivingRead d, derivingEq d,
	derivingOrd d, derivingBounded d, derivingIx d ]

data Deriving = Deriving {
	derivingShow :: Bool, derivingRead :: Bool,
	derivingEq :: Bool, derivingOrd :: Bool,
	derivingBounded :: Bool, derivingIx :: Bool } deriving Show

toDeriving :: [Name] -> (Deriving, [Name])
toDeriving [] = (Deriving False False False False False False, [])
toDeriving (n : ns) = case n of
	NameShow -> (d { derivingShow = True }, ns')
	NameRead -> (d { derivingRead = True }, ns')
	NameEq -> (d { derivingEq = True }, ns')
	NameOrd -> (d { derivingOrd = True }, ns')
	NameBounded -> (d { derivingBounded = True }, ns')
	NameIx -> (d {derivingIx = True }, ns')
	_ -> (d, n : ns')
	where (d, ns') = toDeriving ns

pattern NameShow, NameRead, NameEq, NameOrd, NameBounded, NameIx :: Name
pattern NameShow <- ((== ''Show) -> True)
pattern NameRead <- ((== ''Read) -> True)
pattern NameEq <- ((== ''Eq) -> True)
pattern NameOrd <- ((== ''Ord) -> True)
pattern NameBounded <- ((== ''Bounded) -> True)
pattern NameIx <- ((== ''Ix) -> True)

-- Show

mkInstanceShow :: String -> [String] -> DecQ
mkInstanceShow nt fs = do
	f <- newName "f"
	vs <- replicateM (length fs) $ newName "v"
	instanceD (cxt []) (conT ''Show `appT` conT (mkName nt)) [
		funD 'showsPrec [
			clause [wildP, varP f] (
				normalB $ pp (nt ++ " {") ...
					mkShowFields nt fs vs ...
					pp "}"
				) [valD (conP (mkName nt) $ varP <$> vs) (normalB $ varE f) []] ] ]

mkShowFields :: String -> [String] -> [Name] -> ExpQ
mkShowFields nt fs ns = foldr (...) (varE 'id) . intersperse (pp ", ")
	$ (<$> zip fs ns) \(f, n) -> pp (lcfirst nt ++ ucfirst f ++ " = ") ... (varE 'showsPrec `appE` litI 11 `appE` varE n)

-- Read

mkInstanceRead :: String -> [String] -> DecQ
mkInstanceRead nt fs = do
	vs <- replicateM (length fs) $ newName "v"
	instanceD (cxt []) (conT ''Read `appT` conT (mkName nt)) [
		valD (varP 'readPrec) (normalB $ varE 'parens .$ varE 'prec `appE` litI 10 `appE` doE ([
			bindS (conP 'Ident [litP $ stringL "Foo"]) $ varE 'lexP,
			bindS (conP 'Punc [litP $ stringL "{"]) $ varE 'lexP] ++
			mkReadFields nt fs vs ++
			[bindS (conP 'Punc [litP $ stringL "}"]) $ varE 'lexP,
			noBindS $ varE 'pure .$ foldl appE (conE $ mkName nt) (varE <$> vs)
			])) []
		]

mkReadFields :: String -> [String] -> [Name] -> [StmtQ]
mkReadFields nt fs vs = intercalate [bindS (conP 'Punc [litP $ stringL ","]) $ varE 'lexP] $ (<$> zip fs vs) \(f, v) -> [
	bindS (conP 'Ident [litP . stringL $ lcfirst nt ++ ucfirst f]) $ varE 'lexP,
	bindS (conP 'Punc [litP $ stringL "="]) $ varE 'lexP,
	bindS (varP v) $ varE 'step `appE` varE 'readPrec ]

-- Eq

mkInstanceEq :: String -> [String] -> DecQ
mkInstanceEq nt fs = do
	s1 <- newName "s1"
	s2 <- newName "s2"
	instanceD (cxt []) (conT ''Eq `appT` conT (mkName nt)) [
		funD '(==) [
			clause [varP s1, varP s2] (normalB
				$ foldl (.&&) (conE 'True) $ fieldEqual s1 s2 nt <$> fs
				) []
			]
		]

fieldEqual :: Name -> Name -> String -> String -> ExpQ
fieldEqual s1 s2 nt f_ = (f `appE` varE s1) .== (f `appE` varE s2)
	where
	f = varE . mkName $ lcfirst nt ++ ucfirst f_

-- Ord

mkInstanceOrd :: String -> [String] -> DecQ
mkInstanceOrd nt fs_ = do
	s1 <- newName "s1"
	s2 <- newName "s2"
	instanceD (cxt []) (conT ''Ord `appT` conT (mkName nt)) [
		funD '(<=) [
			clause [varP s1, varP s2] (normalB
				$ varE 'foldr `appE` lamOrd s1 s2 `appE`
					conE 'True `appE` listE fs
				) []
			]
		]
	where fs = varE . mkName . (lcfirst nt ++) . ucfirst <$> fs_

lamOrd :: Name -> Name -> ExpQ
lamOrd s1 s2 = do
	x <- newName "x"
	v <- newName "v"
	lamE [varP x, varP v] $ (varE x `appE` varE s1) .< (varE x `appE` varE s2) .||
		(((varE x `appE` varE s1) .== (varE x `appE` varE s2)) .&& varE v)

-- Bounded

mkInstanceBounded :: String -> [String] -> DecQ
mkInstanceBounded nt fs =
	instanceD (cxt []) (conT ''Bounded `appT` conT (mkName nt)) [
		valD (varP 'minBound) (normalB $ foldl appE (conE $ mkName nt)
			(replicate (length fs) (varE 'minBound))) [],
		valD (varP 'maxBound) (normalB $ foldl appE (conE $ mkName nt)
			(replicate (length fs) (varE 'maxBound))) [] ]

mkInstanceIx :: String -> [String] -> DecQ
mkInstanceIx nt fs = instanceD (cxt []) (conT ''Ix `appT` conT (mkName nt)) [
	mkIxRange 'range nt fs,
	mkIxIndex 'index nt fs,
	mkIxInRange 'inRange nt fs ]

mkIxRange :: Name -> String -> [String] -> DecQ
mkIxRange fn nt fs = do
	(vs, ws, is) <- unzip3
		<$> replicateM (length fs) ((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn [clause [tupP [conP (mkName nt) $ varP <$> vs, conP (mkName nt) $ varP <$> ws]] (normalB
			. compE $ (++ [noBindS . foldl appE (conE $ mkName nt) $ varE <$> is])
				$ (\(i, (v, w)) -> bindS (varP i) $ varE 'range `appE` tupE [varE v, varE w])
					<$> is `zip` (vs `zip` ws)
			) []
		]

-- Ix

mkIxIndex :: Name -> String -> [String] -> DecQ
mkIxIndex fn nt fs = do
	(vs, ws, is) <- unzip3
		<$> replicateM (length fs) ((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn [clause
		[	tupP [conP (mkName nt) $ varP <$> vs, conP (mkName nt) $ varP <$> ws],
			conP (mkName nt) $ varP <$> is ]
		(normalB
			$ varE 'foldl `appE` mkIxIndexLam `appE` litE (integerL 0)
				.$ listE (varE <$> vs) `zp` listE (varE <$> ws) `zp` listE (varE <$> is)
			)
		[]]

mkIxIndexLam :: ExpQ
mkIxIndexLam = do
	v <- newName "v"
	z <- newName "z"
	k <- newName "k"
	lamE [varP v, tupP [varP z, varP k]]
		$ (varE 'index `appE` varE z `appE` varE k) .+ (varE 'rangeSize `appE` varE z .* varE v)

mkIxInRange :: Name -> String -> [String] -> DecQ
mkIxInRange fn nt fs = do
	vs <- replicateM (length fs) $ newName "v"
	ws <- replicateM (length fs) $ newName "w"
	is <- replicateM (length fs) $ newName "i"
	funD fn [clause
		[	tupP [conP (mkName nt) $ varP <$> vs, conP (mkName nt) $ varP <$> ws],
			conP (mkName nt) $ varP <$> is ]
		(normalB . foldr (.&&) (conE 'True)
			$ (\((v, w), i) -> varE 'inRange `appE` tupE [varE v, varE w] `appE` varE i)
				<$> vs `zip` ws `zip` is
			)
		[]]

---------------------------------------------------------------------------
-- STRUCT WITH PRIMITIVE MONAD
---------------------------------------------------------------------------

-- FUNCTION STRUCT PRIM

structPrim :: String -> Name -> Name -> [Name] -> DecsQ
structPrim nt cp fr ds = sequence [
	mkNewtypePrim nt ds,
	mkTypeST nt, mkTypeIO nt,
	mkFreezeSig nt, mkFreezeFun nt cp fr,
	mkThawSig nt, mkThawFun nt cp fr,
	mkCopySig nt, mkCopyFun nt cp fr ]

-- NEWTYPE AND TYPE SYNONYM

mkNewtypePrim :: String -> [Name] -> DecQ
mkNewtypePrim nt ds = do
	s <- newName "s"
	newtypeD (cxt []) (mkName $ nt ++ "Prim") [plainTV s] Nothing (normalC (mkName $ nt ++ "Prim") [
		bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''ForeignPtr `appT` conT (mkName nt)) -- (conT (mkName $ nt ++ "Prim") `appT` varT s))
		]) [derivClause Nothing $ conT <$> ds]

mkTypeIO :: String -> DecQ
mkTypeIO nt = tySynD (mkName $ nt ++ "IO") [] $ conT (mkName $ nt ++ "Prim") `appT` conT ''RealWorld

mkTypeST :: String -> DecQ
mkTypeST nt = tySynD (mkName $ nt ++ "ST") [] . conT . mkName $ nt ++ "Prim"

-- FREEZE

mkFreezeSig :: String -> DecQ
mkFreezeSig nt = do
	m <- newName "m"
	sigD (mkName $ lcfirst nt ++ "Freeze") . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m) .-> (varT m `appT` conT (mkName nt))

mkFreezeFun :: String -> Name -> Name -> DecQ
mkFreezeFun nt frz fr = do
	ff <- newName "ff"
	funD (mkName $ lcfirst nt ++ "Freeze") [
		clause [conP (mkName $ nt ++ "Prim") [varP ff]] (
			normalB (mkFreezeBody nt frz fr ff)
			) []
		]

mkFreezeBody :: String -> Name -> Name -> Name -> ExpQ
mkFreezeBody nt frz fr ff = (varE 'unsafeIOToPrim ... (conE (mkName $ nt ++ "_") `pt` varE '(<$>)))
	.$ (varE 'withForeignPtr `appE` varE ff `appE` varE frz) .>>= (varE 'newForeignPtr .<$> varE 'id .<*> varE fr)

-- THAW

mkThawSig :: String -> DecQ
mkThawSig nt = do
	m <- newName "m"
	sigD (mkName $ lcfirst nt ++ "Thaw") . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName nt) .-> (varT m `appT` (conT (mkName $ nt ++ "Prim") `appT` (conT ''PrimState `appT` varT m)))

mkThawFun :: String -> Name -> Name -> DecQ
mkThawFun nt frz fr = do
	ff <- newName "ff"
	funD (mkName $ lcfirst nt ++ "Thaw") [
		clause [conP (mkName $ nt ++ "_") [varP ff]] (
			normalB (mkThawBody nt frz fr ff)
			) []
		]

mkThawBody :: String -> Name -> Name -> Name -> ExpQ
mkThawBody nt frz fr ff = (varE 'unsafeIOToPrim ... (conE (mkName $ nt ++ "Prim") `pt` varE '(<$>)))
	.$ (varE 'withForeignPtr `appE` varE ff `appE` varE frz) .>>= (varE 'newForeignPtr .<$> varE 'id .<*> varE fr)

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

module AddForallT (addForallT) where

import Data.List
import Language.Haskell.TH

addForallT :: Type -> Type
addForallT t = case getTyVarNames t of
	[] -> t
	ns -> ForallT (map PlainTV ns) [] t

getTyVarNames :: Type -> [Name]
getTyVarNames (ForallT tvb _ tp) =
	filter (`notElem` map nameFromTyVar tvb) $
		getTyVarNames tp
getTyVarNames (AppT t1 t2) =
	getTyVarNames t1 `union` getTyVarNames t2
getTyVarNames (VarT n) = [n]
getTyVarNames _ = []

nameFromTyVar :: TyVarBndr -> Name
nameFromTyVar (PlainTV n) = n
nameFromTyVar (KindedTV n _) = n

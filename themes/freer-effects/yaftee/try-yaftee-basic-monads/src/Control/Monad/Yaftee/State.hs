{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.State where

import GHC.TypeLits
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union

-- * NORMAL

-- * NAMED

type Named nm s = Union.FromFirst (Named_ nm s)

data Named_ (nm :: Symbol) s a where
	Get :: Named_ nm s s; Put :: forall nm s . !s -> Named_ nm s ()

getN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => Eff.E effs i o s
getN nm = Eff.eff (Get @nm)

putN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => s -> Eff.E effs i o ()
putN nm = Eff.eff . Put @nm

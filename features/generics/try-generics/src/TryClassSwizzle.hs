{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryClassSwizzle where

import qualified GHC.Generics as G

import SwizzleGen

deriving instance G.Generic (a, b, c, d, e, f, g, h)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i, j)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i, j, k)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i, j, k, l)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i, j, k, l, m)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
deriving instance G.Generic (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

-- concat <$> classSwizzle `mapM` [1 .. 26]
concat <$> classSwizzle `mapM` [1 .. 15]

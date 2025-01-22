{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SwizzleSetClass2 where

import Try.SwizzleSetClass2.TH

concat <$> classSwizzle `mapM` [1 .. 4]

concat <$> instanceSwizzleTuple `mapM` [1 .. 4]

xyzt "xy"
xyzt "xyz"

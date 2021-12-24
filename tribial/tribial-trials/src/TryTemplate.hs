{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryTemplate where

import Foreign.ForeignPtr

import Template

data Hoge = Hoge_ (ForeignPtr Hoge)

(: []) <$> deriveStorable "Hoge" 10 8

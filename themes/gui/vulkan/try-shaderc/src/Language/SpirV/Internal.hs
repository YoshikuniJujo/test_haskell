{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Language.SpirV.Internal where

import Data.String
import Data.ByteString qualified as BS

import Language.SpirV.ShaderKind

newtype S (sknd :: ShaderKind) = S BS.ByteString deriving (Show, IsString)

writeFile :: FilePath -> S sknd -> IO ()
writeFile fp (S bs) = BS.writeFile fp bs

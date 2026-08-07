{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Bech32.ByteString (encode, decode) where

import Control.Monad.Identity
import Data.ByteString qualified as BS
import Codec.Bech32 qualified as Bech32

encode :: String -> Bech32.B -> Either String BS.ByteString
encode hrp0 = runIdentity . Bech32.switch
	[(pure . (== hrp0), pure . Right)]
	(const . pure . Left $ "HRP should be " ++ show hrp0)

decode :: String -> BS.ByteString -> Bech32.B
decode hrp = Bech32.B hrp

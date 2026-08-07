{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Bech32.ByteString (encode, decode) where

import Data.ByteString qualified as BS
import Codec.Bech32 qualified as Bech32

encode :: String -> Bech32.B -> Either String BS.ByteString
encode hrp0 = Bech32.switch
	[((== hrp0), Right)] (const . Left $ "HRP should be " ++ show hrp0)

decode :: String -> BS.ByteString -> Bech32.B
decode hrp = Bech32.B hrp

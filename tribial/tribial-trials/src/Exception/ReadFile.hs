{-# LANGUAGE ImportQualifiedPost #-}	
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Exception.ReadFile where

import Control.Exception
import Data.ByteString qualified as BS

catchReadFile :: FilePath -> IO (Either String BS.ByteString)
catchReadFile fp = (Right <$> BS.readFile fp)
	`catch` (\(e :: IOError) -> pure . Left $ show e)

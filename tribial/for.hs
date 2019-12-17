{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-missing-signatures #-}

-- import Control.Monad (forM_)
import Data.Foldable (for_)

colors = ["red", "green", "blue", "yellow"]

-- main = forM_ colors print
-- main = for_ colors print
main = for_ colors \c -> do
	print c

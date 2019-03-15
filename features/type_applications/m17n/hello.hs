{-# LANGUAGE TypeApplications, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Messages
import SetLang
import LangYj ()

main :: IO ()
main = do
	putStrLn $ whatsYourName @Lang

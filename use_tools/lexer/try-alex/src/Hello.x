{

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Hello where

import Data.Bool

}

%wrapper "monadUserState"

tokens :-

	$white* "{-#"			{ \_ _ -> pure Curly }
	$white+				{ \_ _ -> pure White }

{

data Token
	= White | Curly
	| Eof
	deriving (Show, Eq)

alexEOF :: Alex Token
alexEOF = pure Eof

data AlexUserState = AlexUserState deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

tryLex :: String -> Either String [Token]
tryLex = (`runAlex` lexAll)

lexAll :: Alex [Token]
lexAll = do
	t <- alexMonadScan
	bool ((t :) <$> lexAll) (pure []) (t == Eof)

}

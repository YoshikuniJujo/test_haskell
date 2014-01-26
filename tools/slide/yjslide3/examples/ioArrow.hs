import Control.Arrow

type IOA = Kleisli IO

getCharA :: IOA () Char
getCharA = Kleisli $ const getChar

putCharA :: IOA Char ()
putCharA = Kleisli putChar

getLineA :: IOA () String
getLineA = Kleisli $ const getLine

putStrLnA :: IOA String ()
putStrLnA = Kleisli putStrLn

main :: IO ()
main = flip runKleisli () $ getLineA >>> putStrLnA

ite :: Bool -> a -> a -> a
ite True t _ = t
ite False _ e = e

-- some :: IOA ()
-- some = getCharA >>> (arr (== '\n')) &&& putCharA >>>
--	arr (\(b, 

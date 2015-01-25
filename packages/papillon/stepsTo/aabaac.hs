import Control.Monad

type Parse v = String -> Maybe (v, String)

a :: Parse String
{-
a ('a' : cs) = case a cs of
	Just (s, 'b' : cs') -> Just ('a' : s ++ "b", cs')
	Just (s, 'c' : cs') -> Just ('a' : s ++ "c", cs')
	_ -> Nothing
	-}
a ('a' : cs) = msum [
	case a cs of
		Just (s, 'b' : cs') -> Just ('a' : s ++ "b", cs')
		_ -> Nothing,
	case a cs of
		Just (s, 'c' : cs') -> Just ('a' : s ++ "c", cs')
		_ -> Nothing ]
a s = Just ("", s)

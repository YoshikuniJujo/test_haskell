class Automaton a where
	step :: a s i -> s -> i -> s
	startState :: a s i -> s
	acceptStates :: a s i -> [s]

run :: Automaton a => a s i -> [i] -> s
run a = foldl (step a) $ startState a

isAccept :: (Automaton a, Eq s) => a s i -> [i] -> Bool
isAccept a = (`elem` acceptStates a) . run a

data Machine s i = Machine (s -> i -> s) s [s]

instance Automaton Machine where
	step (Machine s _ _) = s
	startState (Machine _ s _) = s
	acceptStates (Machine _ _ ass) = ass

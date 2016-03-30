nonsense :: String -> [(String, Int)] -> String
nonsense k d = case lookup k d of
	Just n -> s ++ reverse s
		where s = show n
	_ -> "NO VALUE"

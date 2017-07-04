{-# LANGUAGE Rank2Types #-}

type Robot = ((String, Integer, Integer) -> Integer) -> Integer

robot :: (String, Integer, Integer) -> Robot
robot (_name,_attack,_hp) = \ cmd -> cmd (_name,_attack,_hp)

hp :: (String, Integer, a) -> a
hp (_,_,h) = h

getHp :: Robot -> Integer
getHp aRobot = aRobot hp

setHp :: Robot -> Integer -> Robot
setHp aRobot newHp = aRobot (\ (n,a,_) -> robot (n,a,newHp))

-- damage :: Robot -> Integer -> Robot
{--
damage ::
	(((String, Integer, Integer) -> Integer) -> Integer) -> Integer ->
	((String, Integer, Integer) -> Integer) -> Integer
damage aRobot amount = let actualHp = getHp aRobot
                        in
                        setHp aRobot (actualHp - amount)
	--}


-- makeKiller :: Robot
makeKiller = robot ("Killer",10,200)

-- makeBetty :: Robot
makeBetty = robot ("Betty",5,300)

----- Example of computation in ghci

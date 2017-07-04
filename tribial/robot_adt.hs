data Robot = Robot String Integer Integer

robot :: (String, Integer, Integer) -> Robot
robot (_name,_attack,_hp) = Robot _name _attack _hp

hp :: (String, Integer, Integer) -> Integer
hp (_,_,h) = h

getHp :: Robot -> Integer
getHp (Robot _ _ h) = h

setHp :: Robot -> Integer -> Robot
setHp (Robot n a _) newHp = Robot n a newHp

damage :: Robot -> Integer -> Robot
damage aRobot amount = let actualHp = getHp aRobot
                        in
                        setHp aRobot (actualHp - amount)


makeKiller :: Robot
makeKiller = robot ("Killer",10,200)

makeBetty :: Robot
makeBetty = robot ("Betty",5,300)

----- Example of computation in ghci

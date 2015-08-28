main=mapM putStrLn$zipWith3(\f b->max(f++b).show)(cycle["Fizz",[],[]])(cycle["Buzz",[],[],[],[]])[1..100]

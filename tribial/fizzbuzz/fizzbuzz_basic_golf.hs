main=putStr.unwords$[1..100]>>= \n->return$case(n`mod`3,n`mod`5)of(0,0)->"FizzBuzz";(0,_)->"Fizz";(_,0)->"Buzz";_->show n

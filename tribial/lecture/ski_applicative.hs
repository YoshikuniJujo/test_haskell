-- not work

import Control.Applicative

s = (<*>)

k = pure

i = s k k

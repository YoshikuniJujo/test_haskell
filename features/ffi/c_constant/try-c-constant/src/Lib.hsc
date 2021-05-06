module Lib where

import Data.Int

#include "foo.h"

fooError, fooTwo :: #{type Foo}
fooError = #{const FOO_ERROR}
fooTwo = #{const FOO_TWO}

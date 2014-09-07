{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE  TypeFamilies #-}
--{-# LANGUAGE  #-}

import Data.Fix
import Mon

data LangF a n
 = Id a | Br n n

data Stat = Print String | Read String


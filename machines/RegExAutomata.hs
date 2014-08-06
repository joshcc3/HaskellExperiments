import Control.Applicative
import Machine

type RegExAut  = Moore Char St
data St        = A | N deriving (Eq, Show)

terminal c = Moore N $ \ch -> if ch == c then pure A else terminal c


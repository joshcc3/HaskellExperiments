module Control.Coroutine.FRP where

import Control.Coroutine
import Control.Arrow
import Control.Monad
import qualified Control.Category as C
import Data.Bifunctor (bimap)

{- Co-routines are a type of stream processor. We get a stream of events and the co-routine processes the event with some function. We can think of a unit as an object that behaves like an arrow. The units position depends on keyboard events and the position of other co-routines it is co-operating with. the collisions depend on the units position. -}

type Event a = [a]


-- Map events into different kinds of events
mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

-- Filter events based on a predicate function
filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

-- Replace every event occurence with a fixed event
constE :: e -> Coroutine (Event e') (Event e)
constE = mapE . const

constC :: a -> Coroutine b a
constC = arr . const

idC :: Coroutine a a
idC = arr id

-- Merge two time varying values using a combining function
zipWithC :: (a -> b -> c) -> Coroutine (a, b) c
zipWithC = arr . uncurry

-- Merge two event streams together
zipE :: Coroutine (Event e, Event e) (Event e)
zipE = zipWithC (++)


scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i where
    step a e = let a' = foldl f a e in (a', scanE f a')


-- | Split a value into (current value, previous value) using the given
--   initial value as the previous value during first call.
withPrevious :: a -> Coroutine a (a, a)
withPrevious first = Coroutine $ \i -> ((i, first), step i) where
    step old = Coroutine $ \i -> ((i, old), step i)

-- | Delay the value by a single time-step, using the given initial value for
--   the first call.
delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

integrate :: Num a => a -> Coroutine a a
integrate a = Coroutine (\i -> (a, integrate (a+i)))


-- | Derivate a numerical value over time (i.e. return the delta between current
--   and previous time-step.
derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipWithC (-)

-- | Trigger an event whenever the value satisfies the given predicate function
watch :: (a -> Bool) -> Coroutine a (Event a)
watch f = Coroutine $ \i ->
    if f i
        then ([i], watch f)
        else ([], watch f)


restartWhen :: Coroutine a b -> Coroutine (a, Event e) b
restartWhen co = Coroutine (step co)
  where
    step :: Coroutine a b -> (a, Event e) -> (b, Coroutine (a, Event e) b)
    step co (a, e) | null e    = (b, Coroutine (step co'))
                   | otherwise = (b, Coroutine (step co))
      where
        (b, co') = runC co a


joinC :: (b -> c -> d) -> Coroutine a b -> Coroutine a c -> Coroutine a d
joinC f c c' = c &&& c' >>> arr (uncurry f)



-- we want to create a coroutine that will accept streams of co-routines and an input that must be split across the co-routines and produce as a result the folding of the values for one time step. For the next time step it can choose to either progress with the current or get a switch to a new set of parallel computations.           
-- what i want is something that will take join streams and when specified
-- will
-- its something that joins streams of coroutines

{- 
We want to create a co-routine that will either start a new series of parallel computations or continue them
 (|||) :: a b d -> a c d -> a (Either b c) d
-}


parallelize :: b -> (b -> b -> b) -> [Coroutine a b] -> Coroutine a b
parallelize b f cs = loop $ second (delay cs) >>> arr g
  where
--    g :: (a, [Coroutine a b]) -> (b, [Coroutine a b])
    g (a, cs') = foldl func (b, []) cs'
      where
--        func :: (b, [Coroutine a b]) -> Coroutine a b -> (b, [Coroutine a b])
        func (b',cs) c =  bimap (f b') (:cs) (runC c a)



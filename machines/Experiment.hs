{-# LANGUAGE TupleSections #-}
import Conway
import Moore
import Control.Monad.State
import Control.Lens hiding (lmap)
import Control.Comonad
import Control.Applicative
import Data.Profunctor (lmap)
import Control.Arrow
import Test.QuickCheck

data Move = Rock | Paper | Scissors deriving (Eq, Ord, Show, Enum)
type Strategy = [Move] -> Move

type Player = Moore Move Move
-- Is this a homomorphism? A functor of some sort, but the domain and target categories are different from Hask
morph :: Strategy -> Player
morph strat = fmap strat accum


winningMove Rock = Paper
winningMove Paper = Scissors
winningMove Scissors = Rock

strat1 :: Strategy 
strat1 ms = winningMove . snd $ maximum table
    where 
      table = foldl f init ms 
      init = zip (repeat 0) [Rock .. Scissors]
      f t m = t & ix (fromEnum m) . _1 %~ (+1)

strat2 :: Strategy 
strat2 [] = Rock
strat2 m = winningMove . head $ m

strat3 :: Move -> Strategy
strat3 m = const m

oppStrat :: Strategy -> Strategy
oppStrat = fmap winningMove 

oppOppStrat :: Strategy -> Strategy
oppOppStrat = fmap (winningMove . winningMove)

win :: Move -> Move -> Ordering
win Paper x = compare Paper x
win x Paper = x `compare` Paper
win a b = b `compare` a

switch = fmap ((> 3) . length . filter (==GT)) . zipWith win

-- figure out a way behind the delay hack
feedback1 :: Moore Move Move -> Moore Move (Int, Int)
feedback1 m = go (0,0) (fmap g (win <$> (identity Rock) <*> delay Rock m))
    where 
      g EQ = (0, 0)
      g GT = (1, 0)
      g LT = (0, 1)
      go s (Moore a f) = Moore (a +: s) (go (a +: s) . f)

(a, b) +: (c, d) = (a + c, b + d)

strat5 = higherOrder (morph (strat3 Scissors)) (map morph [strat1, strat2])

seq1 = [Rock, Rock, Rock, Rock, Paper, Paper, Paper, Scissors, Paper, Scissors, Paper]

{-
Oh, this is a very interesting morphism.
We're converting between structures,
we're moving from a list into a tree
Another idea that we can look at over here, instead of using the monad
The comonad, can be used to come up with a strategy using the local information
at that place,
the monad can be used to choose join the notion of the comonad

There must be some way I can describe this in english which will allow me to describe it in haskell
I also though about using the comonad to decide the next move. 
-}


higherOrder :: Moore Move Move -> [Moore Move Move] -> Moore Move Move
higherOrder a b = joinMoore (z a b (y (higherOrder a b)))
    where 
      y :: Moore Move Move -> Moore Move Bool
      y m = switch <$> accum <*> delay [] (accuml m)
      z :: Moore Move Move -> [Moore Move Move] -> Moore Move Bool -> Moore Move (Moore Move Move)
      z d l m = go d l m
          where 
            go d [] m = pure d
            go _ (x:xs) (Moore True f) = Moore x (go x xs . f)
            go x ls (Moore False f) = Moore x (go x ls . f)


loeb :: Functor f => f (f a -> a) -> f a
loeb x = fmap ($ loeb x) x

--ho2 :: Moore a (Moore a b -> b) 
--ho2 

sp = [(!! 2), (+) <$> (!! 0) <*> (!! 2), const 4]



dep lm = Moore lm (\(m, lm') -> dep (m:lm'))

dep' (Moore m f) lm = Moore lm (\(m', lm') -> dep' (f m') (m:lm'))

regulate :: (b -> (a, c) -> c) -> Moore (a, c) b -> Moore (a, c) c -> Moore (a, c) c
regulate g (Moore b f) (Moore b' f') = Moore b' ((regulate g <$> f <*> f') . (fst &&& g b))
      
tie :: Moore (a, c) c -> Moore a c
tie (Moore c f) = Moore c (fmap tie (flip (curry f) c))

combine :: Moore Move Bool -> Moore Move Move -> Moore Move [Move]
combine c m = tie (regulate g c' m')
    where 
      c' :: Moore (Move, [Move]) Bool
      c' = lmap fst c
      m' = dep' m []
      g b (_, c) = if b then [] else c


toMoore :: a -> [a] -> Moore b a
toMoore a [] = pure a
toMoore a (x:xs) = Moore x (const (toMoore a xs))


higherOrder :: Moore Move Move -> [Moore Move Move] -> Moore Move Move
higherOrder a b = joinMoore (z a b (y (higherOrder a b)))
    where 
      y :: Moore Move Move -> Moore Move Bool
      y m = switch <$> combine  (dep [] <*> delay [] (accuml m)
      z :: Moore Move Move -> [Moore Move Move] -> Moore Move Bool -> Moore Move (Moore Move Move)
      z d l m = go d l m
          where 
            go d [] m = pure d
            go _ (x:xs) (Moore True f) = Moore x (go x xs . f)
            go x ls (Moore False f) = Moore x (go x ls . f)




{-
Ok, so the goal of this rock paper scissors game is to come up with strategies.
It's easy to progressively come up with more complicated strategies and keep 
going in that direction.
The assumption that we've been making so far is that we have no extra knowledge,
i.e. when we are trying to come up with a strategy there is the assumption that
we don't have any knowledge of what the opponent might do and we attempt to 
guess the opponents move from his past moves. 
However we could add more background knowledge to the way we handle this game.
For example, create a machine that would choose a strategy given that we know which strategy the user
might be using at a particular point in time
Then extend that using the notion of uncertainty of him using multiple possible strategies at a specific point in time
using []
-}







--treeify1 :: Moore Move Move -> [Moore Move Move] -> Moore Move (Moore Move Move)
--treeify1 m [] = 
--    where 
      


{-
So the goal is to switch between strategies.
We need to make the strategies you're switching between opaque.
There should be only some minimal way of consuming a strategy.
Ok so a strategy is opaque in the sense that we can figure out how many 
times we have won and lost
-}


{-
One use of moore machines:
  imagine you were designing a game and wanted to have this notion of 
  a parallel universe that was governed by a completely different set of rules
  or a parallel universe where you were never born, so all your actions 
  would be null but everyone elses would be fine and then you were randomly
  introduced into the game
  So this captures the notion of several systems developing simultaneously 
  and you choose to only view one of them at a time.
  Much like Quantum physics: What? This makes no sense until you actually choose to test this idea out, what do you even know about quantum physics that you are making such an assumtpion

 Just had the feeling of being in a situation where you are part of a much larger mass and events proceed independent of you. And you are directly involved in them and because of that when you are about to embark up on a new journey, it's very exciting.
-}


{-
For a start let's consider a reactionary system governed by some rules.
One transition is a second.
-}



{-
  So joinMoore is a little restrictive in that it causes us to progress the 
  smaller worlds in the exact same way as the larger worlds.
  The larger world has to bear the self same similarity to the smaller worlds.
  Or the perfect case is if you have to debug a program and you want to try 
  different variations of rules and see which one is the best
  Assumption: we thought we had to use the monad instance, but the monad 
  instance was simply the gateway to understanding how to solve these problems.
  So the monad instance restricts us to having our rules use the same rules
  inside as well as out.
  But what added power does this really give us. 
  My question is, what would you use the monad instance of a moore machine for.
  Ahh, so the monad instance makes this machine composable.
  We can define several smaller machines that simply do one thing or do one idea
  of a thing and then glue them together using a larger machine.
  But wait, that's not the complete story. At the same time, the rules governing
  the smaller machine are a function of some input type. 
  The rules governing the input of the larger machine must be of the same input
  type. For some reason I'm equating type with purpose. 
  So for example a descriptino of a transition like, {Next, Stop, Wait}
  Aha, so simply put, this does not apply to all domains. 
   In the case of a cellular automata of course
   Can crack puns over here
   I see so the semantics have to be the same, therefore the charactersitics 
   must remain the same as far as distinguishing them goes.
   Next: Given some state and some input, go to the next state.
   A state is simply a function
   This monad could also apply to a strategy game.
   like rock paper scissors, 
   we have several strategies that work on the principle that we know
   what the opponent has played. 
   
   
-}


-- So if we want to create a higher order strategy that uses the 
{-

As far as I see it, each transition corresponds to 
I want the higher order machine to be a machine that
decides which machine to use.
The goal is to define some heuristics for knowing when
to switch strategies. 
One machine that we can build is the machine that 
works by choosing a strategy,
then when we know the opponent has caught onto the strategy
switching to the oppOpp, then when the opponent has figured
that out switches to the next one on the stack.

No. Now we want to create a machine that can learn this process
and generate these machines further.
Any fool can make things bigger and more complicated.
It takes a stroke of genius to move in the opposite direction.

-}

--93 47 10 42

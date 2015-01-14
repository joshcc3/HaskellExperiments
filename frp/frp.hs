{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"


import Reactive.Banana
--import Reactive.Banana.WX
import System.Random
import qualified Data.IntMap as I
import Reactive.Banana.Frameworks

{-

1st goal:
Develop a simple clock that counts bsaed on some granularity
So we emit time update events every x seconds
-}











type Loc = Int
type Pos = (Loc, Loc)

type Region = Pos -> Bool
type RegionId = Int

plane :: Region
plane = const True

r0 = rectRegion (0, 10) (5, 0)
r1 = rectRegion (5, 10) (10, 0)
r2 = rectRegion (10, 10) (15, 0)
r3 = rectRegion (15, 10) (20, 0)

regions = [r0, r1, r2, r3]

rectRegion :: Pos -> Pos -> Region
rectRegion ul lr = \p -> p > ul && p < lr

regionMap :: I.IntMap Region
regionMap = I.fromList $ zip [0..] regions


-- similar to how we define reactiv behaviours as functions of time
-- we can define shapes as functions of time.
-- thus the game board is a fairly complex shape over time
-- we need to create a set of primitives for creating such interactive 
-- and reactive shapes. But we shall defer this until we have a better 
-- understanding of how reactive behaviours work. In order to do so, 
-- we will have to better implement the reactive behaviours over here.

-- thus our reactive behaviours over here are:
-- the user clicking events
-- the position of a row.

data Row = Row

type Click = Pos

-- so we have to generate a list of user click events

type Clicks t = Event t Click

type GameBoard t a = Loc -> Behavior t a


{- 

   Right, so the formulation we are going to use is as follows.
   The game board is the inifinite two dimensional plane with some decorations
   we define this using our notion of space.
   Now we need to define a notion of space. The co-domain is dependent on the 
   Thus the game board:
   is a function from location to a time varying value, this value is fixed
   based on the type of game being played.
   The game board doesn't move, instead the user moves over the game board
   The user moves over the game board and is also a reactive value that reacts
   to both the position and the time.
   

-}

{-
Goal - develop the piano tiles game.
The piano tile game consists of a stream of rows.
Each row must have one black cell chosen randomly from among the 4.
The position of the rows is a function of time.

The user provides a series of touch events.
If a row crosses a bound on the board without it being played then the user is out


-}

{-
we imagine the game board as an infinite plane and the user can touch anywhere.

-} 


{-# LANGUAGE Arrows #-}
import Coroutine
import FRP
import Control.Monad
import Control.Arrow


{-
Over here a co-routine is a routine that enacts the behavious of the component. We model a component as a stream processor (an arrow). We have some time varying values such as the player position. Traditionally this is a function from Time -> Position. We can discretize the time and represent it as a stream thus every packet is the value of the function (Time -> Position) at the time = position of the packet. The stream processor will take these inputs and produce an output from them. More specifically from the point of view of the pong game, the players position is dependent on its previous position and the current speed. In fact position is the integration of the speed + the start position.
-}

type Pos           = (Int, Int)
type BallPos       = Pos
type PlayerPos     = Pos
data KeyboardEvent = UP | DOWN | NONE
type Vector        = (Int, Int)
data BallBounce    = VBounce | HBounce
type Rect          = (Int, Int, Int, Int)

player1Y           = 20
player1X           = 10
playerVel          = 2
startBall          = (50,50)
initBallVel        = (10,10)
cieling            = 0
flr                = 500
rightWall          = 750
leftWall           = 0
plWidth            = 5
plHeight           = 10

playerPos :: Coroutine KeyboardEvent PlayerPos
playerPos = playerSpeed >>> integrate player1Y >>> (arr (\y -> (player1X, y)))

playerSpeed :: Coroutine KeyboardEvent Int
playerSpeed = arr func
  where
   func UP   = -playerVel
   func DOWN =  playerVel
   func NONE =  0
   

{- We want to model the ball. The ball constantly moves in some direction until it either hits a horizontal wall or the players racket in which case it bounces back in. if it hits the vertical wall then the player loses. Lets tackle the problem of the horizontal walls and the players racket first. The ball depends on its previous position, its current direction and the position of the player. Similar to the player its speed is a vector. Thus we integrate the balls position. The ballPos is the integration of the ball speed. We perform vector integration individually over the functions of the x and y co-ordinates. Treating the x and y co-ordinates as parallel streams and integrating them individually -}


ballPos :: Coroutine PlayerPos BallPos
ballPos = loop $ arr (\(ppos, bpos) -> ((ppos, bpos), bpos))
          >>> batBounce *** wallE
          >>> zipE
          >>> ballSpeed
          >>> vecIntegrate startBall
          >>> withPrevious initBallVel


{-
ballPos :: Coroutine (Event BallBounce) Pos
ballPos = ballSpeed >>> (vecIntegrate startBall)

Problem with the above formulation is that in order to know the events we need to 
know the ball position thus we find it hard to start off
The above ballPos Coroutine handles this problem in the following way by using arrow recursion.
We make the ballPos an instance of ArrowLoop which has a single function loop which has type,
Coroutine (b,d) (c,d) -> Coroutine b c, This function produces a coroutine that takes a b, and produces a c using the input coroutine. The d that is also required as input is the result of one step of the co-routine. The 'd' not be used in the current iteration.
-}
vecIntegrate :: Pos -> Coroutine Vector Pos
vecIntegrate (x,y) = (integrate x *** integrate y)

bounce :: Vector -> BallBounce -> Vector
bounce (dx,dy) e 
  = case e of
      HBounce -> (-dx, dy)
      VBounce -> (dx, -dy)

ballSpeed :: Coroutine (Event BallBounce) Vector
ballSpeed = scanE bounce initBallVel

{-
In order to generate the events
-}

wallE :: Coroutine BallPos (Event BallBounce)
wallE = watch (\(_,y) -> y < cieling || y > flr) >>> constE VBounce

batBounce :: Coroutine (PlayerPos, BallPos) (Event BallBounce)
batBounce = watch collision >>> constE HBounce

collision :: (PlayerPos, BallPos) -> Bool
collision ((bx, by), (px, py)) =    bx <= px + plWidth 
                                 && by >= py 
                                 && by <= py + plHeight


resetBallPos :: Coroutine PlayerPos BallPos
resetBallPos = loop $ restartWhen ballPos >>> arr id &&& watch outOfBounds 
  where
    outOfBounds (x,_) = x < leftWall || x > rightWall


game :: Coroutine KeyboardEvent [Rect]
game = playerPos >>> arr id &&& resetBallPos >>> (arr $ \(ppos, bpos) -> map makeRect [ppos,bpos])

makeRect :: Pos -> Rect
makeRect = undefined
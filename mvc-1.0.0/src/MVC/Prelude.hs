{-| Simple utilities

    The \"Example\" section at the bottom of this module contains an extended
    example of how to interact with the @sdl@ library using the @mvc@ library
-}

module MVC.Prelude (
    -- * Controllers
      producer
    , stdinLines
    , inLines
    , inRead
    , tick

    -- * Views
    , consumer
    , stdoutLines
    , outLines
    , outShow

    -- * Handles
    , inHandle
    , outHandle

    -- * Example
    -- $example
    ) where

import Control.Applicative (pure, (<*))
import Control.Concurrent.Async (withAsync)
import Control.Concurrent (threadDelay)
import Data.IORef (newIORef, readIORef, writeIORef)
import MVC
import Pipes.Internal (Proxy(..), closed)
import qualified Pipes.Prelude as Pipes
import qualified System.IO as IO

{-| Create a `Controller` from a `Producer`, using the given `Buffer`

    If you're not sure what `Buffer` to use, try `Single`
-}
producer :: Buffer a -> Producer a IO () -> Managed (Controller a)
producer buffer prod = managed $ \k -> do
    (o, i, seal) <- spawn' buffer
    let io = do
            runEffect $ prod >-> toOutput o
            atomically seal
    withAsync io $ \_ -> k (asInput i) <* atomically seal
{-# INLINABLE producer #-}

-- | Read lines from standard input
stdinLines :: Managed (Controller String)
stdinLines = producer Single Pipes.stdinLn
{-# INLINABLE stdinLines #-}

-- | Read lines from a file
inLines :: FilePath -> Managed (Controller String)
inLines filePath = do
    handle <- inHandle filePath
    producer Single (Pipes.fromHandle handle)
{-# INLINABLE inLines #-}

-- | 'read' values from a file, one value per line, skipping failed parses
inRead :: Read a => FilePath -> Managed (Controller a)
inRead filePath = fmap (keeps parsed) (inLines filePath)
  where
    parsed k str = case reads str of
        [(a, "")] -> Constant (getConstant (k a))
        _         -> pure str
{-# INLINABLE inRead #-}

-- | Emit empty values spaced by a delay in seconds
tick :: Double -> Managed (Controller ())
tick n = producer Single $ lift (threadDelay (truncate (n * 1000000))) >~ cat
{-# INLINABLE tick #-}

-- | Create a `View` from a `Consumer`
consumer :: Consumer a IO () -> Managed (View a)
consumer cons0 = managed $ \k -> do
    ref <- newIORef cons0
    k $ asSink $ \a -> do
        cons <- readIORef ref
        let go cons_ = case cons_ of
                Request () fa -> writeIORef ref (fa a)
                Respond v  _  -> closed v
                M          m  -> m >>= go
                Pure    r     -> writeIORef ref (return r)
        go cons
{-# INLINABLE consumer #-}
    
-- | Write lines to standard output
stdoutLines :: View String
stdoutLines = asSink putStrLn
{-# INLINABLE stdoutLines #-}

-- | Write lines to a file
outLines :: FilePath -> Managed (View String)
outLines filePath = do
    handle <- outHandle filePath
    return (asSink (IO.hPutStrLn handle))
{-# INLINABLE outLines #-}

-- | 'show' values to a file, one value per line
outShow :: Show a => FilePath -> Managed (View a)
outShow filePath = fmap (contramap show) (outLines filePath)
{-
outShow filePath = do
    handle <- outHandle filePath
    return (asSink (IO.hPrint handle))
-}
{-# INLINABLE outShow #-}

-- | Read from a `FilePath` using a `Managed` `IO.Handle`
inHandle :: FilePath -> Managed IO.Handle
inHandle filePath = managed (IO.withFile filePath IO.ReadMode)
{-# INLINABLE inHandle #-}

-- | Write to a `FilePath` using a `Managed` `IO.Handle`
outHandle :: FilePath -> Managed IO.Handle
outHandle filePath = managed (IO.withFile filePath IO.WriteMode)
{-# INLINABLE outHandle #-}

{- $example
    The following example distils a @sdl@-based program into pure and impure
    components.  This program will draw a white rectangle between every two
    mouse clicks.

    The first half of the program contains all the concurrent and impure logic.
    The `View` and `Controller` must be `Managed` together since they both share
    the same initialization logic:

> import Control.Monad (join)
> import Graphics.UI.SDL as SDL
> import Lens.Family.Stock (_Left, _Right)  -- from `lens-family-core`
> import MVC
> import MVC.Prelude
> import qualified Pipes.Prelude as Pipes
> 
> data Done = Done deriving (Eq, Show)
> 
> sdl :: Managed (View (Either Rect Done), Controller Event)
> sdl = join $ managed $ \k -> withInit [InitVideo, InitEventthread] $ do
>     surface <- setVideoMode 640 480 32 [SWSurface]
>     white   <- mapRGB (surfaceGetPixelFormat surface) 255 255 255
> 
>     let done :: View Done
>         done = asSink (\Done -> SDL.quit)
> 
>         drawRect :: View Rect
>         drawRect = asSink $ \rect -> do
>             _ <- fillRect surface (Just rect) white
>             SDL.flip surface
> 
>         totalOut :: View (Either Rect Done)
>         totalOut = handles _Left drawRect <> handles _Right done
> 
>     k $ do
>         totalIn <- producer Single (lift waitEvent >~ cat)
>         return (totalOut, totalIn)

    Note the `Control.Monad.join` surrounding the `managed` block.  This is
    because the type before `Control.Monad.join` is:

> Managed (Managed (View (Either Rect Done), Controller Event))

    More generally, note that `Managed` is a `Monad`, so you can use @do@
    notation to combine multiple `Managed` resources into a single `Managed`
    resource.

    The second half of the program contains the pure logic.

> pipe :: Monad m => Pipe Event (Either Rect Done) m ()
> pipe = do
>     Pipes.takeWhile (/= Quit) >-> (click >~ rectangle >~ Pipes.map Left)
>     yield (Right Done)
> 
> rectangle :: Monad m => Consumer' (Int, Int) m Rect
> rectangle = do
>     (x1, y1) <- await
>     (x2, y2) <- await
>     let x = min x1 x2
>         y = min y1 y2
>         w = abs (x1 - x2)
>         h = abs (y1 - y2)
>     return (Rect x y w h)
> 
> click :: Monad m => Consumer' Event m (Int, Int)
> click = do
>     e <- await
>     case e of
>         MouseButtonDown x y ButtonLeft ->
>             return (fromIntegral x, fromIntegral y)
>         _ -> click
> 
> main :: IO ()
> main = runMVC () (asPipe pipe) sdl

    Run the program to verify that clicks create rectangles.

    The more logic you move into the pure core the more you can exercise your
    program purely, either manually:

>>> let leftClick (x, y) = MouseButtonDown x y ButtonLeft
>>> Pipes.toList (each [leftClick (10, 10), leftClick (15, 16), Quit] >-> pipe)
[Left (Rect {rectX = 10, rectY = 10, rectW = 5, rectH = 6}),Right Done]

    ... or automatically using property-based testing (such as @QuickCheck@):

>>> import Test.QuickCheck
>>> quickCheck $ \xs -> length (Pipes.toList (each (map leftClick xs) >-> pipe)) == length xs `div` 2
+++ OK, passed 100 tests.

    Equally important, you can formally prove properties about your model using
    equational reasoning because the model is `IO`-free and concurrency-free.
-}

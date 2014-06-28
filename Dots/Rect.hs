module Dots.Rect where

import Control.Arrow
import Control.Monad
import Graphics.Rendering.OpenGL hiding (Rect, Size)
import qualified Data.Map as M

data Shape = Rectangle Pos Size | Circle Pos R 
type Pos = (X,Y)
type Size = (W,H)
type X = Int
type Y = Int
type W = Int
type H = Int
type R = Int
type Shapes = [Shape]

divisions = 24

circle (x, y) radius divs = map toPoint angles where 
    arc       = 2.0 * pi / fromIntegral divs
    toPoint a = (x + cos a * radius, y + sin a * radius)
    angles    = map ((*arc) . fromIntegral) [0..divs]

renderFan points = do
    renderPrimitive TriangleFan $ mapM_ (\(x, y) -> vertex (Vertex2 x y)) points

renderCircle centre radius divs = renderFan (centre : circle centre radius divs)


renderShapes :: Shapes -> IO ()
renderShapes s
 = renderRects (filter isRect s) >> mapM_ f (filter isCircle s)
 where
   isRect (Rectangle _ _) = True
   isRect _ = False
   isCircle (Circle _ _) = True
   isCircle _ = False
   f (Circle (x, y) r) = renderCircle (fromIntegral x :: GLfloat, fromIntegral y :: GLfloat) (fromIntegral r) divisions

renderRects rects = do
    currentColor $= Color4 0.9 0.9 0.9 1.0
    let rectVertices (Rectangle (x,y) (w,h)) = do
            vertex $ vtx x y
            vertex $ vtx (x+w) y
            vertex $ vtx (x+w) (y+h)
            vertex $ vtx x (y+h)

    renderPrimitive Quads $ mapM_ rectVertices rects


vtx :: Int -> Int -> Vertex2 GLfloat
vtx x y = Vertex2 (fromIntegral x) (fromIntegral y)



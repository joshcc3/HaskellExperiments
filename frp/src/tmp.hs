{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Graphics.Gloss

main = animate (InWindow "Gloss" (700,700) (0,0))
           black picture

picture :: Float -> Picture
picture 0 = text "Value cannot be 0"
picture number = scale 6.5 6.5 (color rose $ drawpicture number)

orangered, orangered2, orangered3 :: Color
orangered = makeColor 1.0 0.251 0.0 0.7
orangered2 = makeColor 1.0 0.251 0.0 0.5
orangered3 = makeColor 1.0 0.251 0.0 0.3

intervalsmall = [0,11.25,22.5,33.75,45,56.25,67.5,78.75]
intervalbig = [0,22.5,45,67.5,90,112.5,135,157.5,180,202.5,225,247.5,270,292.5,315,337.5]
xlist = [2,4..50]
ylist = [0,2..48]

squares = pictures[rotate x (line [(-50,0),(0,50),(50,0),(0,-50),(-50,0)]) | x <- intervalsmall]
stars = pictures[rotate x ((pictures [line [(-8.5,0),(0,50),(8.5,0)],line[(0,50),(0,0)]])) | x <- intervalbig]
grid = pictures[line [(0,y),(x,50)] | x <- xlist, y <- ylist, x-y==2]
insidegrid = pictures[
    translate 0 (-50) grid,
    rotate 90 (translate 0 (-50) grid),
    rotate 180 (translate 0 (-50) grid),
    rotate 270 (translate 0 (-50) grid)]

rotVal :: Float -> Float
rotVal x = x - (x / (2*pi))

drawpicture :: Float -> Picture
drawpicture number = pictures [
    rot $ color red (pictures [circle 50,circle 8.5]),
    line [(-50,-50),(-50,50),(50,50),(50,-50),(-50,-50)],
    rot $ squares,
    rot $ scale 0.7 0.7 squares,
    rot $ scale 0.49 0.49 squares,
    rot $ scale 0.347 0.347 squares,
    rot $ scale 0.242 0.242 squares,
    rot $ color orange stars,
    rot (color orange (scale 0.178 0.178 stars)),
    rot (rotate 11.25 (scale 0.178 0.178 stars)),
    translate (-50) 0 grid,
    rotate 90 (Translate (-50) 0 grid),
    rotate 180 (Translate (-50) 0 grid),
    rotate 270 (Translate (-50) 0 grid),
    rot $ color orangered insidegrid,
    rot $ color orangered2 (rotate 45 insidegrid),
    rot $ color orangered3 (rotate 22.5 insidegrid),
    rot $ color orangered3 (rotate 67.5 insidegrid)
    ]
  where rot = rotate (rotVal number)

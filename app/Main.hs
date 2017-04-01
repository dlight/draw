module Main where

import Graphics.Gloss

import Point
import Fractal

animationStep :: Float
animationStep = 0.1 -- seconds

draw :: Int -> [Segment] -> Picture
draw max segments = Pictures (map f (take max segments))
  where f (Segment p1 p2) = Line [p p1, p p2]
        p (Point p1 p2) = (p1, p2)

produceFrame :: [Segment] -> Float -> Picture
produceFrame segments time =
  let maxSegments = floor $ time / animationStep in
    draw maxSegments segments

main :: IO ()
main = animate (InWindow
                "Drawing"
                (600, 600)
                (200, 40))
       white
       (produceFrame segmentsToDraw)

module Fractal(segmentsToDraw) where

import Point

-- Fractal is a list of line segments, plus a list of other fractals.
-- This should model fractal trees and Sierpinski triangles
data Fractal = Fractal [Segment] [Fractal] deriving Show

fractalShallowSegments :: Fractal -> [Segment]
fractalShallowSegments (Fractal segments _) = segments

fractalChildren :: Fractal -> [Fractal]
fractalChildren (Fractal _ children) = children

translateFractal :: Point -> Fractal -> Fractal
translateFractal p (Fractal s children) = Fractal
  (map (translateSegment p) s)
  (map (translateFractal p) children)

rotateFractal :: Rotation -> Fractal -> Fractal
rotateFractal rotation (Fractal segments children) = Fractal
  (map (rotateSegment rotation) segments)
  (map (rotateFractal rotation) children)

transformFractal :: Isometry -> Fractal -> Fractal
transformFractal (Isometry rotation translation) fractal =
  translateFractal translation (rotateFractal rotation fractal)

treeFractal :: Point -> Float -> Float -> Angle -> Fractal
treeFractal start length shrinkFactor (Angle theta) =
  Fractal [Segment start end] [left, right]
  where end = translatePoint (Point 0 length) start
        nextTree :: Fractal
        nextTree = treeFractal
          start
          (length * shrinkFactor)
          shrinkFactor
          (Angle theta)

        left :: Fractal
        left = positionNext (Angle (-theta))
        right :: Fractal
        right = positionNext (Angle theta)

        positionNext :: Angle -> Fractal
        positionNext angle = transformFractal
          (Isometry (Rotation start angle) (Point 0 length))
          nextTree

-- breadth-first search (first shallow, then deep)
-- http://aryweb.nl/2013/10/28/haskell-tree-traversal/

segmentsOfFractal :: Fractal -> [Segment]
segmentsOfFractal t = build [t]
  where
    build :: [Fractal] -> [Segment]
    build [] = []
    build xs = shallow ++ deep
      where
        shallow :: [Segment]
        shallow = concatMap fractalShallowSegments xs

        deep :: [Segment]
        deep = build (concatMap fractalChildren xs)

---
--- Here's what's actually exported:
---

segmentsToDraw :: [Segment]
segmentsToDraw = segmentsOfFractal myFractal
  where
    myFractal :: Fractal
    myFractal = treeFractal
      (Point 0 0)
      100
      0.5
      (Angle (pi / 4))

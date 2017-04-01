module Point where
-- This module defines much more than points..

-- 2D point
-- Trouble: I represent translations as points too.
-- Perhaps I should have a separate data Vector = Vector Float Float
data Point = Point Float Float deriving Show

-- Line segment
data Segment = Segment Point Point deriving Show

-- Angle in radians
data Angle = Angle Float deriving Show

-- Rotation around a point
data Rotation = Rotation Point Angle deriving Show

-- Rotation then translation
data Isometry = Isometry Rotation Point deriving Show

mirrorPoint :: Point -> Point
mirrorPoint (Point x y) = Point (- x) (- y)

translatePoint :: Point -> Point -> Point
translatePoint (Point x y) (Point x' y') =
  Point (x + x') (y + y')

rotatePointOrigin :: Angle -> Point -> Point
rotatePointOrigin (Angle theta) (Point x y) = Point
  (x * cos theta - y * sin theta)
  (y * cos theta + x * sin theta)

rotatePoint :: Rotation -> Point -> Point
rotatePoint (Rotation point' angle) point = movedBack
  where
    movedBack :: Point
    movedBack = translatePoint rotated point'
    rotated :: Point
    rotated = rotatePointOrigin angle movedToOrigin
    movedToOrigin :: Point
    movedToOrigin = translatePoint point (mirrorPoint point')

transformPoint :: Isometry -> Point -> Point
transformPoint (Isometry rotation translation) point =
  translatePoint translation (rotatePoint rotation point)

translateSegment :: Point -> Segment -> Segment
translateSegment p (Segment p1 p2) = Segment
  (translatePoint p p1)
  (translatePoint p p2)

rotateSegment :: Rotation -> Segment -> Segment
rotateSegment rotation (Segment p1 p2) = Segment
  (rotatePoint rotation p1)
  (rotatePoint rotation p2)

transformSegment :: Isometry -> Segment -> Segment
transformSegment (Isometry rotation translation) segment =
  translateSegment translation (rotateSegment rotation segment)

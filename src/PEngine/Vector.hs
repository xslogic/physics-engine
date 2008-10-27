module PEngine.Vector where

data Vector = Vector {
      x :: Float,
      y :: Float,
      z :: Float
    } deriving Show
      

scale                  :: Float -> Vector -> Vector
scale s (Vector x y z) = Vector (s * x) (s * y) (s * z)

magnitude                :: Vector -> Float
magnitude (Vector x y z) = sqrt $ (x * x) + (y * y) + (z * z)


normalize   :: Vector -> Vector
normalize v = scale f v 
    where f = (1 / magnitude v)


(<+>)                                     :: Vector -> Vector -> Vector
(<+>) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
    
(<->)       :: Vector -> Vector -> Vector
(<->) v1 v2 = v1 <+> (scale (-1) v2)

(<.>)                                     :: Vector -> Vector -> Float
(<.>) (Vector x1 y1 z1) (Vector x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

(<*>)                                     :: Vector -> Vector -> Vector
(<*>) (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (y1 * z2 - y2 * z1) (x2 * z1 - x1 * z2) (x1 * y2 - x2 * y1)
                                            





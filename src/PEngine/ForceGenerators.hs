module PEngine.ForceGenerators where

import PEngine.Vector
import PEngine.Particle
import Data.IORef

data StandardForceGenerator = GravityFG 
                            | SpringFG {
                                otherParticle :: Particle,
                                const :: Float,
                                restLength :: Float
                              }
                            | AnchoredSpringFG {
                                anchor :: Position,
                                const :: Float,
                                restLength :: Float
                              }
                            | BungeeSpringFG {
                                other :: Particle,
                                const :: Float,
                                restLength :: Float
                              }
                            | BuoyancyFG {
                                waterHeight :: Float,
                                maxDepth :: Float,
                                volume :: Float,
                                liquidDensity :: Float
                              }
                            | DragFG {
                                k1 :: Float,
                                k2 :: Float
                              }


updateForce' part p2 c r isBungee = do
    p1 <- readIORef $ pos part
    let dl = p1 <-> p2
        mag = abs $ r - magnitude dl
        noChange = if (isBungee)
                     then (r >= magnitude dl)
                     else False
    if (noChange)
       then return ()
       else applyForce part $ scale (-1 * mag * c) $ normalize dl


class ForceGenerator a where
    updateForce :: Time -> Particle -> a -> IO ()

instance (ForceGenerator a) => ForceGenerator [a] where
    updateForce t p xs = mapM_ (updateForce t p) xs


instance ForceGenerator StandardForceGenerator where

    updateForce _ p GravityFG = do
      m <- readIORef $ mass p
      applyForce p (scale 0 g) 

    updateForce _ p (DragFG x y) = do
      v <- readIORef $ vel p
      let speed = magnitude v
          coeff = (x * speed) + (y * speed * speed)
          v' = normalize v
      applyForce p $ scale (-1 * coeff) v'

    updateForce _ p (SpringFG o c r) = do
      p2 <- readIORef $ pos o
      updateForce' p p2 c r False

    updateForce _ p (AnchoredSpringFG a c r) = updateForce' p a c r False

    updateForce _ p (BungeeSpringFG o c r) = do
      p2 <- readIORef $ pos o
      updateForce' p p2 c r True

    updateForce _ p (BuoyancyFG wh md v d) = do
      (Vector pX pY pZ) <- readIORef $ pos p
      if (pY >= wh + md)
         then return ()
         else if (pY <= wh - md)
                then applyForce p $ Vector 0 (d * v) 0
                else applyForce p $ Vector 0 (d * v * (pY - wh - md / (2 * md))) 0

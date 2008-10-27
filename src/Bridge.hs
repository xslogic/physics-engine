module Main where

import Graphics.Rendering.OpenGL as OGL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import Demo
import Int

import PEngine.Vector as V
import PEngine.Particle as P
import PEngine.World as W
import PEngine.Contact as C

data Bridge = Bridge {
      world :: W.ParticleWorld,
      cables :: [C.ParticleLink],
      rods :: [C.ParticleLink],
      supports :: [C.ParticleLink],
      massPos :: IORef P.Position,
      masDisplayPos :: IORef P.Position
    }

baseMass = 1
extraMass = 10

getPos' :: Int -> P.Position
getPos' i = V.Vector (((fromIntegral i)/2) * 2 - 5) 4 $ fromIntegral ((i `mod` 2) * 2 - 1)

createCables' (p0:p1:[]) cbls = cbls
createCables' (p0:p1:p2:ps) cbls = createCables' (p1:p2:ps) 
                                   ((C.Cable p0 (Left p2) 0.3 1.9):cbls) 

createSupports' :: (Particle, Int) -> C.ParticleLink
createSupports' (p, i) = C.Cable p (Right (V.Vector ((fromIntegral i/2) * 2.2 - 5.5) 
                                            4 
                                            ((fromIntegral (i `mod` 2)) * 1.6 - 0.8)
                                      )
                               ) 0.5 (if (i < 6) 
                                         then (fromIntegral i/2) * 0.5 + 3.0
                                         else 5.5 - ((fromIntegral i/2) * 0.5)
                                     )

createRods' [] rs = rs
createRods' (p0:p1:ps) rs = createRods' ps ((C.Rod p0 (Left p1) 2):rs) 

                     
                     

initBridge :: IO (Bridge)
initBridge = do
  pIdRef <- newIORef 0
  pw@(PWorld psRef cgsRef fgmRef mc) <- initWorld pIdRef 12
  ps <- get psRef
  mapM_ (\(p, i) -> updateParticle p 0 (getPos' i) zeroV zeroV 0.9) 
            (zip ps [0..])
  let cbls = createCables' ps []
      sprts = map createSupports' $ zip ps [0..]
      rds = createRods' ps []
      conGens' = (map CG cbls) ++ (map CG sprts) ++ (map CG rds)
  conGens <- get cgsRef
  cgsRef $= (conGens ++ conGens')
  massPosRef <- newIORef (V.Vector 0 0 0.5)
  massDPosRef <- newIORef (V.Vector 0 0 0)
  return $ Bridge pw cbls rds sprts massPosRef massDPosRef

update' :: IORef P.Position -> [P.Particle] -> Float -> Float -> Float -> IO ()
update' mdpRef ps i' f1 f2 = do
  mdp <- get mdpRef
  let i = floor i'
      p = (ps !! i)
  pos' <- get $ P.pos p
  updateMass p (baseMass + (extraMass * f1 * f2))
  mdpRef $= pos' <+> (V.scale (f1*f2) mdp)
  

updateAdditionalMass :: Bridge -> IO ()
updateAdditionalMass (Bridge pw@(PWorld psRef _ _ _) _ _ _ mpRef mdpRef) = do
  ps <- get psRef
  mapM_ (\p -> updateMass p baseMass) ps
  mp@(V.Vector x y z) <- get mpRef
  let xp = x - (fromIntegral (floor x))
      zp = z - (fromIntegral (floor z))
      (x', xp') = if (x < 0)
                   then (0, 0)
                   else if (x >= 5)
                          then (5, 0)
                          else (x, xp)
      (z', zp') = if (z < 0)
                   then (0, 0)
                   else if (z >= 1)
                          then (1, 0)
                          else (z, zp)
  mdpRef $= zeroV
  update' mdpRef ps ((x' * 2) + z') (1 - xp') (1 - zp')
  case ((xp' > 0), (zp' > 0)) of
    (False, False) -> return ()
    (True, False) -> update' mdpRef ps ((x' * 2) + z' + 2) xp' (1 - zp')
    (False, True) -> update' mdpRef ps ((x' * 2) + z' + 1) (1 - xp') zp'
    (True, True) -> do
      update' mdpRef ps ((x' * 2) + z' + 2) xp' (1 - zp')
      update' mdpRef ps ((x' * 2) + z' + 3) xp' zp'
      
  
                     
  
{--  

instance Application Bridge where
    title _ = "Bridge Demo"

 -}


  
  
  


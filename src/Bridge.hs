module Main where

import Graphics.Rendering.OpenGL as OGL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import Demo.Bootstrap
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
getPos' i = V.Vector x' 4 z' 
            where
              x' = fromIntegral ((i `div` 2) * 2 - 5)
              z' = fromIntegral ((i `mod` 2) * 2 - 1)

createCables' (p0:p1:[]) cbls = cbls
createCables' (p0:p1:p2:ps) cbls = createCables' (p1:p2:ps) 
                                   ((C.Cable p0 (Left p2) 0.3 1.9):cbls) 

createSupports' :: (Particle, Int) -> C.ParticleLink
createSupports' (p, i) = let iDiv = fromIntegral (i `div` 2)
                             iMod = fromIntegral (i `mod` 2) in
                         C.Cable p (Right (V.Vector ((iDiv * 2.2) - 5.5) 
                                                6 
                                                ((iMod * 1.6) - 0.8)
                                          )
                                   ) 0.5 (if (i < 6) 
                                            then (iDiv * 0.5) + 3.0
                                            else 5.5 - (iDiv * 0.5)
                                         )
                                         

createRods' [] rs = rs
createRods' (p0:p1:ps) rs = createRods' ps ((C.Rod p0 (Left p1) 2):rs) 

                     
                     

initBridge :: IO (Bridge)
initBridge = do
  pIdRef <- newIORef 0
  pw@(PWorld psRef cgsRef fgmRef mc) <- initWorld pIdRef 12
  ps <- get psRef
  mapM_ (\(p, i) -> updateParticle p 0 (getPos' i) zeroV g 0.9) 
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
  mdpRef $= (mdp <+> (V.scale (f1*f2) pos'))
{--
  pos'' <- get mdpRef
  putStrLn $ "Index : " ++ (show i)  
               ++ " Pos : " ++ (show pos'')
               ++ " Pos' : " ++ (show pos')
-}

fmodf x = if (x > 0)
            then (fromIntegral (floor x), x - (fromIntegral (floor x)))
            else (fromIntegral (ceiling x),x - (fromIntegral (ceiling x)))

updateAdditionalMass :: Bridge -> IO ()
updateAdditionalMass (Bridge pw@(PWorld psRef _ _ _) _ _ _ mpRef mdpRef) = do
  ps <- get psRef
  mapM_ (\p -> updateMass p baseMass) ps
  mp@(V.Vector x _ z) <- get mpRef
  let (x0, xp) = fmodf x
      (z0, zp) = fmodf z
      (x', xp') = if (x < 0)
                   then (0, 0)
                   else if (x >= 5)
                          then (5, 0)
                          else (x0, xp)
      (z', zp') = if (z < 0)
                   then (0, 0)
                   else if (z >= 1)
                          then (1, 0)
                          else (z0, zp)
{--
  putStrLn $ (show (x, xp)) ++ " : " ++ (show (x', xp'))
           ++ " :: " ++ (show (z, zp)) ++ " : " ++ (show (z', zp'))
-}
  mdpRef $= zeroV
  update' mdpRef ps ((x' * 2) + z') (1 - xp') (1 - zp')
  case ((xp' > 0), (zp' > 0)) of
    (False, False) -> return ()
    (True, False) -> update' mdpRef ps ((x' * 2) + z' + 2) xp' (1 - zp')
    (False, True) -> update' mdpRef ps ((x' * 2) + z' + 1) (1 - xp') zp'
    (True, True) -> do
      update' mdpRef ps ((x' * 2) + z' + 2) xp' (1 - zp')
      update' mdpRef ps ((x' * 2) + z' + 3) xp' zp'
      
  
  
kCallback' :: Bridge -> IORef TimingData -> 
               Key -> KeyState -> Modifiers -> GLUT.Position -> IO ()
kCallback' br _ (Char ch) Down _ _ = do 
  mp@(V.Vector x' y' z') <- get $ massPos br
  let (x'', z'') = case (ch) of
                     's' -> if (z' + 0.1 > 1) then (x', 1) else (x', z' + 0.1) 
                     'w' -> if (z' - 0.1 < 0) then (x', 0) else (x', z' - 0.1) 
                     'a' -> if (x' - 0.1 < 0) then (0, z') else (x' - 0.1, z') 
                     'd' -> if (x' + 0.1 > 5) then (5, z') else (x' + 0.1, z')
                     _ -> (x', z')
  (massPos br) $= (V.Vector x'' y' z'')
kCallback' _ _ k ks _ _ = return ()



render' c@(C.Cable p0 (Left p1) _ _) = do
  pos0@(V.Vector x0 y0 z0) <- get $ pos p0
  pos1@(V.Vector x1 y1 z1) <- get $ pos p1
{--  putStrLn $ (show x0) ++ " : " ++ (show x1) -}
  drawLine' x0 y0 z0 x1 y1 z1
render' r@(C.Rod p0 (Left p1) _) = do
  pos0@(V.Vector x0 y0 z0) <- get $ pos p0
  pos1@(V.Vector x1 y1 z1) <- get $ pos p1
  drawLine' x0 y0 z0 x1 y1 z1
render' s@(C.Cable p0 (Right (V.Vector x1 y1 z1)) _ _) = do
  pos0@(V.Vector x0 y0 z0) <- get $ pos p0
  drawLine' x0 y0 z0 x1 y1 z1

drawLine' x0 y0 z0 x1 y1 z1 = do
  vertex $ Vertex3 x0 y0 z0
  vertex $ Vertex3 x1 y1 z1



instance Application Bridge where
    title _ = "Bridge Demo"

    update br@(Bridge w _ _ _ _ _) tdRef = do
                              update (MassAggApp w) tdRef
                              updateAdditionalMass br

    kmCallback x timerRef = Just $ kCallback' x timerRef 

    display br@(Bridge w cs rs ss mpRef mdpRef) = do
                              display (MassAggApp w)
                              preservingMatrix $ do
                                renderPrimitive Lines $ do
                                                    currentColor $= (Color4 0 0 1 1)
                                                    mapM_ render' cs
                                                    currentColor $= (Color4 0 1 0 1)
                                                    mapM_ render' rs
                                                    currentColor $= (Color4 0.7 0.7 0.7 1)
                                                    mapM_ render' ss
                              preservingMatrix $ do
                                currentColor $= (Color4 1 0 0 1)
                                (V.Vector x' y' z') <- get mdpRef
                                translate $ Vector3 x' (y' + 0.25) z'
                                renderObject Solid $ GLUT.Sphere' 0.25 20 10
      

                                                   
main = do
  br <- initBridge
  bootStrap br (Size 640 320)




  
  
  



module Main where

import Graphics.Rendering.OpenGL as OGL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import Demo.Bootstrap
import Int

import PEngine.Vector as V
import PEngine.Particle as P




data ShotType = Unused
              | Pistol
              | Artillery
              | FireBall
              | Laser
                deriving (Show, Eq)

data AmmoRound = AmmoRound {
      particle :: Particle,
      shotType :: IORef ShotType,
      startTime :: IORef Int64
    } deriving Show


data BallisticApp = BallisticApp {
      myState :: IORef [AmmoRound],
      currentShot :: IORef ShotType
    }



createNew :: ParticleIDGen -> IO (AmmoRound)
createNew genRef = do
  p <- stationaryParticle genRef 0
  sh <- newIORef Unused
  st <- newIORef (0::Int64)
  return AmmoRound {
               particle = p,
               shotType = sh,
               startTime = st
             }


render :: AmmoRound -> IO ()
render a@(AmmoRound p sh _) = do
  am <- get sh
  if (am == Unused)
     then return ()
     else render' p


render' :: Particle -> IO ()
render' part = do
  (V.Vector x y z) <- get $ pos part
  currentColor $= (Color4 0 0 0 0.2)
  preservingMatrix $ do
             translate $ Vector3 x y z
             renderObject Solid $ GLUT.Sphere' 0.3 5 4
  preservingMatrix $ do
             currentColor $= (Color4 0.75 0.75 0.75 1)
             translate $ Vector3 x 0 z
             GLUT.scale (1::GLfloat) 1 1
             renderObject Solid $ GLUT.Sphere' 0.6 5 4


prep :: ShotType -> Int64 -> AmmoRound -> IO ()
prep Pistol t (AmmoRound p sh st) = do
  sh $= Pistol
  updateParticle p 2.0 
                     (V.Vector 0.0 1.5 0.0) 
                     (V.Vector 0.0 0.0 35.0) 
                     (V.Vector 0.0 (-1.0) 0.0) 
                     0.99 
  st $= t
prep Artillery t (AmmoRound p sh st) = do
  sh $= Artillery
  updateParticle p 200.0 
                     (V.Vector 0.0 1.5 0.0) 
                     (V.Vector 0.0 30.0 40.0) 
                     (V.Vector 0.0 (-20.0) 0.0) 
                     0.99 
  st $= t
prep Laser t (AmmoRound p sh st) = do
  sh $= Laser
  updateParticle p 0.1 
                     (V.Vector 0.0 1.5 0.0) 
                     (V.Vector 0.0 0.0 100.0) 
                     (V.Vector 0.0 0.0 0.0) 
                     0.99 
  st $= t
prep FireBall t (AmmoRound p sh st) = do
  sh $= FireBall
  updateParticle p 1.0 
                     (V.Vector 0.0 1.5 0.0) 
                     (V.Vector 0.0 0.0 10.0) 
                     (V.Vector 0.0 6.0 0.0) 
                     0.9 
  st $= t
prep Unused _ _ = return ()


modify :: AmmoRound -> TimingData -> IO ()
modify ammo@(AmmoRound part shRef stRef) td = do
  sh <- get shRef
  if (sh == Unused)
     then return ()
     else modify' part shRef stRef td

modify' part shRef stRef td = do
  st <- get stRef
  pos' <- get $ pos part
  sh <- get shRef
  let duration = ((fromIntegral (lastFrameDuration td))::Float) * 0.001 
      lfTs = getMilli (lastFrameTimeStamp td)
      z' = z pos'
  integrate' part duration
  if ((st < lfTs - 5000) || (z' > 200)) 
    then shRef $= Unused
    else return ()



fire :: ShotType -> Int64 -> [AmmoRound] -> IO ()
fire st t [] = return ()
fire st t (a@(AmmoRound _ sType _):xs) = do
  t' <- get sType
  if (t' == Unused)
     then prep st t a
     else fire st t xs
 


{--
fire' :: ShotType -> Int64 -> [AmmoRound] -> AmmoRound-> [AmmoRound] -> IO ()
fire' _ _ [] _  xs = reverse xs
fire' st t xs (AmmoRound _ Unused _) ys = (reverse ys) ++ [prep st t] ++ xs 
fire' st t (x:xs) y ys = fire' st t xs x (y:ys)  
-}

kmCallback' :: BallisticApp -> IORef TimingData -> 
               Key -> KeyState -> Modifiers -> GLUT.Position -> IO ()
kmCallback' x timerRef (MouseButton LeftButton) Down _ _ = do
  as <- get (myState x)
  td <- get timerRef
  shot <- get (currentShot x)
  let t = getMilli (lastFrameTimeStamp td)
  fire shot t as

kmCallback' x _ (Char '1') Down _ _ = do { print "Pistol"; (currentShot x) $= Pistol }
kmCallback' x _ (Char '2') Down _ _ = do { print "Artillery"; (currentShot x) $= Artillery }
kmCallback' x _ (Char '3') Down _ _ = do { print "FireBall"; (currentShot x) $= FireBall }
kmCallback' x _ (Char '4') Down _ _ = do { print "Laser"; (currentShot x) $= Laser }
kmCallback' _ _ k ks _ _ = return ()

  


instance Application BallisticApp where
    title _ = "Ballistics !!"

    display x = do
      clear [ColorBuffer, DepthBuffer]
      loadIdentity
      lookAt (Vertex3 ((-25.0)::GLdouble) 8.0 5.0) 
                 (Vertex3 0.0 5.0 22.0) 
                 (Vector3 (0.0::GLdouble) 1.0 0.0) 
      preservingMatrix $ do
                       currentColor $= (Color4 0 0 0 1)
                       translate $ Vector3 (0.0::GLdouble) 1.5 0.0
                       renderObject Solid $ GLUT.Sphere' 0.1 5 5
                       translate $ Vector3 (0.75::GLdouble) 0.75 0.75
                       currentColor $= (Color4 0.75 0.75 0.75 1)
                       GLUT.scale (1::GLfloat) 1 1
                       renderObject Solid $ GLUT.Sphere' 0.1 5 5

      currentColor $= (Color4 0.75 0.75 0.75 0.5)
      let vs = [(Vertex3 (-5.0::GLdouble) 0.0 (fromIntegral z)) | z <- [0..199], (z `mod` 10 == 0)]
      renderPrimitive Lines $ 
                      mapM_ (\v@(Vertex3 x y z) -> do 
                               vertex v 
                               vertex (Vertex3 (-1 * x) y z)) vs
      as <- get (myState x)
      mapM_ render as

    update x timerRef = do
      td <- get timerRef
      as <- get (myState x)
      mapM_ (\a -> modify a td) as
      update Basic timerRef
                                    
    kmCallback x timerRef = Just $ kmCallback' x timerRef
    


main = do
  genRef <- newIORef 0
  ammos <- mapM (\_ -> createNew genRef) [1..20]
  aRef <- newIORef ammos
  csRef <- newIORef Laser
  bootStrap (BallisticApp aRef csRef) (Size 640 320)

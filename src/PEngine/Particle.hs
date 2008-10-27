module PEngine.Particle where

import PEngine.Vector
import Data.IORef
import System.IO.Unsafe

type Position = Vector
type Velocity = Vector
type Accel = Vector
type Force = Vector
type Damping = Float
type Time = Float

data Particle = Particle {
      pid :: Int,
      mass :: IORef Float,
      pos :: IORef Position,
      vel :: IORef Velocity,
      acc :: IORef Accel,
      damping :: IORef Damping
    } deriving Show

instance Eq Particle where
    (==) p1 p2 = (pid p1) == (pid p2)

instance Ord Particle where
    compare p1 p2 = compare (pid p1) (pid p2) 

origin = Vector 0 0 0
zeroV = origin
g = Vector 0 (-10) 0


type ParticleIDGen = IORef Int


stationaryParticle :: ParticleIDGen -> Float -> IO (Particle)
stationaryParticle iRef m = do
  mR <- newIORef m
  pR <- newIORef origin
  vR <- newIORef zeroV
  aR <- newIORef zeroV
  dR <- newIORef 0
  i <- readIORef iRef
  writeIORef iRef $ i + 1
  return  $ Particle i mR pR vR aR dR


updateMass (Particle _ mR _ _ _ _) = writeIORef mR
updatePos (Particle _ _ pR _ _ _) = writeIORef pR
updateVel (Particle _ _ _ vR _ _) = writeIORef vR
updateAcc (Particle _ _ _ _ aR _) = writeIORef aR
updateDamping (Particle _ _ _ _ _ dR) = writeIORef dR

updateParticle part m p v a d = do
  updateMass part m
  updatePos part p
  updateVel part v
  updateAcc part a
  updateDamping part d



integrate' :: Particle -> Time -> IO ()
integrate' p t = integrate (Vector 0 0 0) t p


applyForce :: Particle -> Force -> IO ()
applyForce part@(Particle _ mR _ _ aR _) f = do
  a <- readIORef aR
  m <- readIORef mR
  writeIORef aR $ a <+> (scale (1 / m) f) 


integrate       :: Force -> Time -> Particle -> IO ()
integrate f t (Particle _ mR pR vR aR dR) = do
  m <- readIORef mR
  p <- readIORef pR
  v <- readIORef vR
  a <- readIORef aR
  let a' = (a <+> (scale (1 / m) f))
  writeIORef pR $ p <+> (scale t v) <+> (scale (0.5 * t * t) a')
  writeIORef vR $ v <+> (scale t a')
  



instance (Show a) => Show (IORef a) where
    show ref = unsafePerformIO $ do
                 x <- readIORef ref
                 return $ show x
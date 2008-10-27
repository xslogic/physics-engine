module PEngine.Contact where

import PEngine.Vector
import PEngine.Particle
import Data.IORef
import Monad

data ParticleContact = NullContact | Contact {
      p0 :: Particle,
      p1 :: Maybe Particle,
      restitution :: Float,
      penetration :: Float,
      contactNormal :: Vector
    } deriving Show




data ParticleLink = Cable {
      part0 :: Particle,
      part1 :: Particle,
      restitution' :: Float,
      maxLength :: Float
    } | Rod {
      part0 :: Particle,
      part1 :: Particle,
      len :: Float
    }


createContacts :: ParticleLink -> IO [ParticleContact]
createContacts cable@(Cable part0 part1 r mL) = do
  pos0 <- readIORef $ pos part0
  pos1 <- readIORef $ pos part1
  let m = magnitude $ pos0 <-> pos1
  if (mL > m)
      then return []
      else return $ [Contact {
                 p0 = part0,
                 p1 = Just part1,
                 restitution = r,
                 penetration = (m - mL),
                 contactNormal = normalize (pos1 <-> pos0)
               }]
createContacts rod@(Rod part0 part1 mL) = do
  pos0 <- readIORef $ pos part0
  pos1 <- readIORef $ pos part1
  let m = magnitude $ pos0 <-> pos1
      n = normalize (pos1 <-> pos0)
  if (mL == m)
      then return []
      else return $ [Contact {
                 p0 = part0,
                 p1 = Just part1,
                 restitution = 0,
                 penetration = abs (m - mL),
                 contactNormal = if (m > mL) then n else (scale (-1) n)
               }]
                         


calculateSeparatingVelocity :: ParticleContact -> IO (Float)
calculateSeparatingVelocity (Contact p0 p1' _ _ cn) = do
  v0 <- readIORef $ vel p0
  case (p1') of
    Nothing -> return $ v0 <.> cn
    Just p1 -> do
               v1 <- readIORef $ vel p1
               return $ (v0 <-> v1) <.> cn
               
 
totalInverseMass :: ParticleContact -> IO (Float)
totalInverseMass c@(Contact p0 Nothing _ _ _) = do
  m <- readIORef $ mass p0
  return (1 / m)
totalInverseMass c@(Contact p0 (Just p1) _ _ _) = do
  m0 <- readIORef $ mass p0
  m1 <- readIORef $ mass p1
  return $ (1 / m0) + (1 / m1)


accCausedSepVel :: ParticleContact -> Time-> IO (Float)
accCausedSepVel c@(Contact p0 Nothing _ _ cn) t = do
  a <- readIORef $ acc p0
  return $ t * (a <.> cn)
accCausedSepVel c@(Contact p0 (Just p1) _ _ cn) t = do
  a0 <- readIORef $ acc p0
  a1 <- readIORef $ acc p1
  return $ t * ((a0 <-> a1) <.> cn)



applyImpulses :: Particle -> Maybe Particle -> Vector -> IO ()
applyImpulses p0 Nothing im = do
  m <- readIORef $ mass p0
  v <- readIORef $ vel p0
  updateVel p0 $ v <+> (scale (1 / m) im)
applyImpulses p0 (Just p1) im = do
  applyImpulses p0 Nothing im
  applyImpulses p1 Nothing $ scale (-1) im


applyPenetration :: Particle -> Maybe Particle -> Vector -> IO ()
applyPenetration p0 Nothing im = do
  m <- readIORef $ mass p0
  p <- readIORef $ pos p0
  updatePos p0 $ p <+> (scale (1 / m) im)
applyPenetration p0 (Just p1) im = do
  applyPenetration p0 Nothing im
  applyPenetration p1 Nothing im



func' (x, p') p = do
  v <- calculateSeparatingVelocity p
  if (x > v)
     then return (v, p)
     else return (x, p')

resolveContacts :: [ParticleContact] -> Int -> Time -> IO ()
resolveContacts _ 0 _ = return ()
resolveContacts pcs iter t = do
 (m, p) <- foldM func' (0, NullContact) pcs
 resolveContact p t
 resolveContacts pcs (iter - 1) t


resolveContact :: ParticleContact -> Time -> IO ()
resolveContact p t = resolveVelocity p t >> resolveInterPenetration p


resolveVelocity :: ParticleContact -> Time -> IO ()
resolveVelocity c@(Contact p0 p1 r _ cn) t = do
  sV <- calculateSeparatingVelocity c
  if (sV > 0)
     then return ()
     else do
       tim <- totalInverseMass c
       aSV <- accCausedSepVel c t
       let sV' = -1 * sV * r
           sV'' = if (aSV < 0) 
                    then let sV''' = (sV' + (r * aSV))
                         in if (sV''' < 0) then 0 else sV'''
                    else sV'
           d = sV'' - sV
       if (tim <= 0)
          then return ()
          else do
            let impulse = d / tim
                impulsePerIMass = scale impulse cn
            applyImpulses p0 p1 impulsePerIMass
                
           
resolveInterPenetration :: ParticleContact -> IO ()
resolveInterPenetration c@(Contact p0 p1 r p cn) = do
  if (p <= 0)
     then return ()
     else do
       tim <- totalInverseMass c
       if (tim <= 0)
          then return ()
          else do
            let movePerIMass = scale ((-1 * p) / tim) cn
            applyPenetration p0 p1 movePerIMass




class ContactGenerator a where
    generateContacts :: a -> [Particle] -> Int -> IO [ParticleContact]

      
instance ContactGenerator ParticleLink where
    generateContacts a _ _ = createContacts a 
                                                   
                                                   


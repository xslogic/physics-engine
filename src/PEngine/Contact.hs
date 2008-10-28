module PEngine.Contact where

import PEngine.Vector
import PEngine.Particle
import Data.IORef
import Monad

data ParticleContact = NullContact | Contact {
      p0 :: Particle,
      p1 :: Maybe Particle,
      restitution :: Float,
      penetration :: IORef Float,
      contactNormal :: Vector
    } deriving Show




data ParticleLink = Cable {
      part :: Particle,
      anchor :: Either Particle Position,
      restitution' :: Float,
      maxLength :: Float
    } | Rod {
      part :: Particle,
      anchor :: Either Particle Position,
      len :: Float
    } deriving Show



createContacts :: ParticleLink -> IO [ParticleContact]
createContacts cable@(Cable part anchor r mL) = do
  pos0 <- readIORef $ pos part
  pos1 <- case (anchor) of
            Left part1 -> readIORef $ pos part1
            Right pos' -> return pos'
  let m = magnitude $ pos0 <-> pos1
  penRef <- newIORef (m - mL)
  if (mL > m)
      then return []
      else return $ [Contact {
                 p0 = part,
                 p1 = (case (anchor) of
                         Left part1 -> Just part1
                         Right pos' -> Nothing),
                 restitution = r,
                 penetration = penRef,
                 contactNormal = normalize (pos1 <-> pos0)
               }]
createContacts rod@(Rod part anchor mL) = do
  pos0 <- readIORef $ pos part
  pos1 <- case (anchor) of
            Left part1 -> readIORef $ pos part1
            Right pos' -> return pos'
  let m = magnitude $ pos0 <-> pos1
      n = normalize (pos1 <-> pos0)
  penRef <- newIORef $ abs (m - mL)
  if (mL == m)
      then return []
      else return $ [Contact {
                 p0 = part,
                 p1 = (case (anchor) of
                         Left part1 -> Just part1
                         Right pos' -> Nothing),
                 restitution = 0,
                 penetration = penRef,
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


applyPenetration :: Particle -> Maybe Particle -> Vector -> 
                    IO (Vector, Vector)
applyPenetration p0 Nothing im = do
  m <- readIORef $ mass p0
  p <- readIORef $ pos p0
  let move = (scale (1 / m) im)
  updatePos p0 $ p <+> move
  return (move, zeroV)
applyPenetration p0 (Just p1) im = do
  (m0, _) <- applyPenetration p0 Nothing im
  (m1, _) <- applyPenetration p1 Nothing (scale (-1) im)
  return (m0, m1)


func' (x, p') p = do
  v <- calculateSeparatingVelocity p
  pen <- readIORef $ penetration p
  if ((v < x) && ((v < 0) || (pen > 0)))
     then return (v, p)
     else return (x, p')


{-- (ParticleContact part0 (Just part1) _ pen cn) -}
adjustContact' :: ParticleContact -> ParticleContact -> (Vector, Vector) -> IO () 
adjustContact' NullContact _ _ = return ()
adjustContact' _ NullContact _ = return ()
adjustContact' pc0 pc1 (v0, v1) = do
  let (Contact p0 mP1 _ pen cn) = pc0
      (Contact p0' mP1' _ _ _) = pc1
      adjPen = (\p v -> do { penit <- readIORef p; writeIORef p (penit + v) })
  if (p0 == p0') 
     then adjPen pen (-1 * (v0 <.> cn))
     else case (mP1') of
            Just p1' -> if (p0 == p1')
                                  then adjPen pen (-1 * (v1 <.> cn))
                                  else return ()
            Nothing -> return ()
  case (mP1) of
    Just p1 -> if (p1 == p0')
                 then adjPen pen $ v0 <.> cn
                 else case (mP1') of
                        Just p1' -> if (p1 == p1')
                                      then adjPen pen $ v1 <.> cn
                                      else return ()
                        Nothing -> return ()
    Nothing -> return ()
                         

resolveContacts :: [ParticleContact] -> Int -> Time -> IO ()
resolveContacts _ 0 _ = return ()
resolveContacts pcs iter t = do
 (m, pc) <- foldM func' ((1/0), NullContact) pcs
 (m0, m1) <- resolveContact pc t
 mapM_ (\pc' -> adjustContact' pc' pc (m0, m1)) pcs
 resolveContacts pcs (iter - 1) t


resolveContact :: ParticleContact -> Time -> IO (Vector, Vector)
resolveContact p t = do
  resolveVelocity p t 
  resolveInterPenetration p


resolveVelocity :: ParticleContact -> Time -> IO ()
resolveVelocity NullContact _ = return ()
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
                
           
resolveInterPenetration :: ParticleContact -> IO (Vector, Vector)
resolveInterPenetration NullContact = return (zeroV, zeroV)
resolveInterPenetration c@(Contact p0 p1 r pRef cn) = do
  p <- readIORef pRef
  if (p <= 0)
     then return (zeroV, zeroV)
     else do
       tim <- totalInverseMass c
       if (tim <= 0)
          then return (zeroV, zeroV)
          else do
            let movePerIMass = scale (p / tim) cn
            applyPenetration p0 p1 movePerIMass




class ContactGenerator a where
    generateContacts :: a -> [Particle] -> Int -> IO [ParticleContact]

      
instance ContactGenerator ParticleLink where
    generateContacts a _ _ = createContacts a 
                                                   
                                                   


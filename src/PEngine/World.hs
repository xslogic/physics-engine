{-# LANGUAGE ExistentialQuantification #-}
module PEngine.World where

import PEngine.Vector
import PEngine.Particle
import qualified PEngine.Contact as C
import PEngine.ForceGenerators
import Data.IORef
import Data.Map as M
import Monad


data CGen = GroundContactGenerator
          deriving Show

generateContacts' :: Int -> [Particle] -> [C.ParticleContact] -> IO [C.ParticleContact]
generateContacts' _ [] pcs = return pcs
generateContacts' 0 _ pcs  = return pcs
generateContacts' n (p:ps) pcs = do
  pos' <- readIORef $ pos p
  let y' = y pos'
  if (0 > y')
     then generateContacts' (n - 1) ps $ (C.Contact p Nothing 0.2 (-y') (Vector 0 1 0)):pcs
     else generateContacts' n ps pcs

instance C.ContactGenerator CGen where
    generateContacts _ ps limit = generateContacts' limit ps []




data ParticleWorld = forall a b. (C.ContactGenerator a, ForceGenerator b) => PWorld {
      particles :: IORef [Particle],
      contactGens :: IORef [a],
      forceGenMap :: IORef (Map Particle [b]),
      maxContacts :: Int
    }

getContacts' :: (C.ContactGenerator a) => 
                Int -> [Particle] -> [a] -> [C.ParticleContact] -> IO [C.ParticleContact]
getContacts' _ [] _ pcs = return pcs
getContacts' _ _ [] pcs = return pcs
getContacts' 0 _ _  pcs = return pcs
getContacts' l ps (cg:cgs) pcs = do
  pcs' <- C.generateContacts cg ps l
  let l' = length pcs'
  if (l - l' > 0)
     then getContacts' (l - l') ps cgs (pcs' ++ pcs)
     else return (pcs' ++ pcs)

getContacts :: ParticleWorld -> IO [C.ParticleContact]
getContacts (PWorld psRef cgsRef _ l) = do
  ps <- readIORef psRef
  cgs <- readIORef cgsRef
  getContacts' l ps cgs []

integrateWorld :: ParticleWorld -> Time -> IO ()
integrateWorld (PWorld psRef _ _ _) t = do
  ps <- readIORef psRef
  mapM_ (\p -> integrate' p t) ps


updateForces :: ParticleWorld -> Time -> IO ()
updateForces (PWorld psRef _ fgRef _ ) t = do
  fgMap <- readIORef fgRef
  ps <- readIORef psRef
  mapM_ (\p -> f p fgMap t) ps 
    where f = \p fgm t -> case (M.lookup p fgm) of
                            Nothing -> return ()
                            Just fgs -> updateForce t p fgs
  

resolveContacts :: ParticleWorld -> Time -> IO ()
resolveContacts pw t = do
  pcs <- getContacts pw
  let l = (length pcs)
  C.resolveContacts pcs (l * 2) t


runPhysics :: ParticleWorld -> Time -> IO ()
runPhysics pw t = do
  updateForces pw t
  integrateWorld pw t
  resolveContacts pw t
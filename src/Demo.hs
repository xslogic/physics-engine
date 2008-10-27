module Demo where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import Control.Exception
import System.Time
import PEngine.World
import PEngine.Vector as V
import PEngine.Particle as P

import Int

data TimingData = TData {
                   frameNumber :: Int64,
                   lastFrameTimeStamp :: ClockTime,
                   lastFrameDuration :: Int64,
                   averageFrameDuration :: Double,
                   isPaused :: Bool,
                   fps :: Double
                 } deriving Show

getMilli :: ClockTime -> Int64
getMilli (TOD s p) = (fromIntegral ((s * 1000) + fromInteger (p `div` (toInteger 10^9))))::Int64


initTimer :: IO (TimingData)
initTimer = do
   cTime <- getClockTime
   return TData {
                frameNumber = 0,
                lastFrameTimeStamp = cTime,
                lastFrameDuration = 0,
                averageFrameDuration = 0,
                fps = 0,
                isPaused = False
              }

updateTimer :: TimingData -> IO (TimingData)
updateTimer t = do 
  t' <- getClockTime
  evaluate t'
  let dt = (diffClockTimes t' (lastFrameTimeStamp t))
  evaluate dt
  let sec = toInteger (tdSec dt)
      psec = tdPicosec dt
      dPico = psec + (sec * 10^12)
      dMilli = fromInteger (dPico `div` (10^9)) :: Int64
      fn = if (isPaused t) then (frameNumber t) 
           else (1 + (frameNumber t))
      (lfd, afd, fp) = (dMilli, averageFrameDuration t, fps t)
      afd' = if (afd <= 0) then (fromIntegral lfd)::Double
             else (afd * 0.99) + 0.01 * ((fromIntegral lfd)::Double)
      fp' = (1000 / afd') 
  return TData {
               frameNumber = fn,
               lastFrameTimeStamp = t',
               lastFrameDuration = dMilli,
               averageFrameDuration = if (fn <= 1) then afd else afd',
               fps = if (fn <= 1) then fp else fp',
               isPaused = False
             }

    

class Application a where
    
    initGraphics :: a -> IORef Size -> IO ()
    initGraphics x s = do
                      clearColor $= Color4 0.9 0.95 1 1
                      shadeModel $= Smooth
                      setView x s

    setView :: a -> IORef Size -> IO ()
    setView x sizeRef = do
                      matrixMode $= Projection
                      loadIdentity
                      (Size w h) <- get sizeRef
                      perspective 60 ((conv w) / (conv h)) 1.0 500.0
                      matrixMode $= Modelview 0
                     where conv x = (fromIntegral x)::Double
        

        
    display :: a -> IO ()
    display x = do
      clear [ColorBuffer]
      renderPrimitive Lines $ do
                      vertex $ Vertex2 (1::GLfloat) 1
                      vertex $ Vertex2 (639::GLfloat) 319


    resize :: a -> IORef Size -> Size -> IO ()
    resize x sizeRef (Size width height) = do 
                      sizeRef $= s
                      viewport $= (p, s)
                      setView x sizeRef
                    where height' = if (height <= 0) then 1 else height
                          p = Position 0 0
                          s = Size width height'

    updateAppTimer :: a -> IORef TimingData -> IO()
    updateAppTimer x tData = do
                      oldTData <- get tData
                      newTData <- updateTimer oldTData
                      evaluate $ foldl (+) 0 [1..9999] {-- Dont know what else to do.. -}
                      tData $= newTData       


    update :: a -> IORef TimingData -> IO ()
    update x t = do
      updateAppTimer x t
      postRedisplay Nothing

    kmCallback :: a -> IORef TimingData -> Maybe KeyboardMouseCallback
    kmCallback _ _ = Nothing

    mouseDragCallback :: a -> IORef TimingData -> Maybe MotionCallback
    mouseDragCallback _ _ = Nothing


    title :: a -> String


createWindow' :: String -> IO Window
createWindow' t = do
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  initialWindowSize $= Size 640 320
  initialWindowPosition $= Position 0 0
  createWindow t

bootStrap :: (Application a) => a -> Size -> IO ()
bootStrap x s = do
  (progName,_) <- getArgsAndInitialize
  sizeRef <- newIORef s
  window <- createWindow' (title x)

  lighting $= Enabled
  lightModelAmbient $= (Color4 1.0 1.0 1.0 1.0)

  reshapeCallback $= Just (resize x sizeRef)
  displayCallback $= do 
                display x
                flush
                swapBuffers

  timer <- initTimer
  timerRef <- newIORef timer
  keyboardMouseCallback $= kmCallback x timerRef
  idleCallback $= Just (update x timerRef)
  motionCallback $= mouseDragCallback x timerRef
  initGraphics x sizeRef
  mainLoop
     

data BaseApp = Basic

instance Application BaseApp where
    title _ = "Base Application"


data MassAggApp = MassAggApp ParticleWorld

instance Application MassAggApp where
    title _ = "Mass Aggregate Appliation"
    
    display mApp@(MassAggApp pw@(PWorld psRef _ _ _)) = do
                                  clear [ColorBuffer, DepthBuffer]
                                  loadIdentity
                                  lookAt (Vertex3 0.0 3.5 8.0)
                                             (Vertex3 0.0 5.0 22.0)
                                             (Vector3 (0.0::GLdouble) 1.0 0.0)
                                  ps <- get psRef
                                  mapM_ (\p -> preservingMatrix $ do
                                                 (V.Vector x' y' z') <- get $ P.pos p
                                                 currentColor $= (Color4 0 0 0 1)
                                                 translate $ Vector3 x' y' z'
                                                 renderObject Solid $ GLUT.Sphere' 0.1 20 10
                                                 ) ps

    update (MassAggApp pw) tdRef = do
      td <- get tdRef
      let duration = ((fromIntegral (lastFrameDuration td))::Float) * 0.001
      if (duration <= 0.0)
         then return ()
         else runPhysics pw duration
      update Basic tdRef
      


                                  




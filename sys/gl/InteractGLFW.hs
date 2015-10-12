import Data.Maybe (isJust)
import Control.Monad
import Control.Exception

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import System.IO
import System.Exit
import Text.Printf

type Distance = GL.GLfloat
type Angle = GL.GLfloat
data StateChange
    = Move (Distance,Distance)   -- (forward,rightward)
    | Look Angle                 -- look by 'angle' upward
    | Turn Angle                 -- turn by 'angle' rightword
    | Quit
  deriving Show

data WorldState = WorldState {
    win :: GLFW.Window,
    posX, posY :: Distance,
    angRL, angUD :: Angle,
    isDoomed :: Bool
}

int :: (Integral a, Num b) => a -> b
int = fromIntegral

flt :: (Fractional b, Real a) => a -> b
flt = realToFrac

{-
c_f2d :: GL.GLfloat -> GL.GLdouble
c_f2d = flt . float2Double . flt
-}

glVertex3f :: (GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GL.Vertex3 GL.GLfloat
glVertex3f (x,y,z) = GL.Vertex3 x y z

{-
glVector3f :: (GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GL.Vector3 GL.GLfloat
glVector3f (x,y,z) = GL.Vector3 x y z
-}

glColor4f :: (GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GL.Color4 GL.GLfloat
glColor4f (r,g,b,a) = GL.Color4 r g b a

glVertex3d :: (GL.GLdouble, GL.GLdouble, GL.GLdouble) -> GL.Vertex3 GL.GLdouble
glVertex3d (x,y,z) = GL.Vertex3 x y z

glVector3d :: (Double, Double, Double) -> GL.Vector3 GL.GLdouble
glVector3d (x,y,z) = GL.Vector3 (realToFrac x) (realToFrac y) (realToFrac z)

{-
glColor4d :: (GL.GLdouble, GL.GLdouble, GL.GLdouble, GL.GLdouble) -> GL.Color4 GL.GLdouble
glColor4d (r,g,b,a) = GL.Color4 r g b a
-}

toMicroseconds :: Double -> Int
toMicroseconds t = truncate $ 1000000 * t

main :: IO ()
main = do
  GLFW.setErrorCallback (Just cbErr)
  ok <- GLFW.init
  unless ok $ error "init failed"

  -- Just _mon <- GLFW.getPrimaryMonitor

  mw <- GLFW.createWindow 640 480 "InteractGL" Nothing Nothing
  let w = case mw of
            Nothing -> error "createWindow failed"
            Just v -> v

  GLFW.makeContextCurrent mw

  chan <- newTChanIO :: IO (TChan StateChange)

  -- GLFW.windowHint $ GLFW.WindowHint'Visible True
  GLFW.setKeyCallback w (Just $ cbKey chan)
  GLFW.setCharCallback w (Just $ cbChar chan)
  GLFW.setWindowCloseCallback w (Just cbClose)

  GLFW.swapInterval 1
  -- GLFW.enableKeyRepeat

  GL.depthFunc $= Just GL.Less

  let initworld = WorldState {
      win = w,
      posX = 0, posY = 0,
      angRL = (-pi)/4, angUD = 0,
      isDoomed = False
  }
  (mainLoop initworld chan) `finally` (quit w)


mainLoop :: WorldState -> TChan StateChange -> IO ()
mainLoop world chan = do
  world' <- handleEvents chan world
  if isDoomed world'
  then return ()
  else do
    Just t0 <- GLFW.getTime
    draw world'
    Just t1 <- GLFW.getTime
    let dt = (t0 + spf - t1)
    when (dt > 0) $ threadDelay (toMicroseconds dt)
    mainLoop world' chan
  where fps = 60
        spf = recip fps

handleEvents chan world = do
  emptyChan <- atomically $ isEmptyTChan chan
  if emptyChan then return world
  else do
    msg <- atomically $ readTChan chan
    print msg >> hFlush stdout   -- debug
    handleEvents chan $ case msg of
      Move (fwd,side) -> moveView (fwd,side) world
      Look angUD -> turnViewUD angUD world
      Turn angRL -> turnViewRL angRL world
      Quit -> world { isDoomed = True }


draw world = do
  (w, h) <- GLFW.getWindowSize (win world)

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.viewport  $= (GL.Position 0 0, GL.Size (int w) (int h))
  GL.matrixMode $= GL.Projection
  let ratio = (int w / int h)
  GL.loadIdentity
  GL.frustum (-ratio) ratio (-1.0) 1.0 1.8 30

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  let ang = angRL world
      eye = glVertex3d (flt $ posX world, flt $ posY world, 1.8)
      at = glVertex3d (flt $ posX world + cos ang, 
                       flt $ posY world - sin ang,
                       flt $ 1.8 + sin (angUD world))
      up = glVector3d (0, 0, 1)
  GL.lookAt eye at up

  GL.renderPrimitive GL.Quads $ do
      let a = 20.0
      forM_ [(0,0), (a,0), (a,a), (0,a)] $ \(x, y) ->
          let vtx = glVertex3f (x,y,0)
              col = glColor4f (x/a, 1.0 ,y/a, 1.0)
          in GL.color col >> GL.vertex vtx

  printErrors
  GLFW.swapBuffers (win world)
  GL.flush
  GLFW.pollEvents

printErrors = GL.get GL.errors >>= mapM_ print

quit _w = GLFW.terminate

moveView (fwd,aside) w =
    w { posX = dX + posX w, posY = dY + posY w }
  where dX = fwd * cos ang - aside * cos (ang - pi/2)
        dY = (-fwd) * sin ang + aside * sin (ang - pi/2)
        ang = angRL w

turnViewUD ang w = w { angUD = ang'' }
  where ang'' = if ang' > pi/2 then pi - ang'
                else if ang' < (-pi/2) then pi + ang'
                else ang'
        ang' = ang + angUD w

turnViewRL ang w = w { angRL = ang'' }
  where ang'' = if ang' > pi then 2 * pi - ang'
                else if ang' < (-pi) then 2 * pi + ang'
                else ang'
        ang' = ang + angRL w

cbChar chan win c = do
  let step = 0.5
  let cacts = [ ('w', Move (step,0)), ('s', Move ((-step),0)) ]
  putStrLn $ printf "%c is pressed" c
  hFlush stdout
  case lookup c cacts of
    Just act -> atomically $ writeTChan chan act
    _ -> return ()

cbKey chan win key n kst mods = do
  let tangle = 0.02
  let kacts = [ (GLFW.Key'Escape, Quit),
                (GLFW.Key'Left,   Turn (-tangle)),
                (GLFW.Key'Right,  Turn tangle),
                (GLFW.Key'Down,   Look (-tangle)),
                (GLFW.Key'Up,     Look tangle) ]
  case lookup key kacts of
    Just act -> atomically $ writeTChan chan act
    _ -> return ()

cbResize w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (int w) (int h))
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

cbErr err msg = do
  print err

cbClose _w = GLFW.terminate >> exitSuccess

import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.UI.GLFW as GLFW

type Distance = GL.GLfloat
type Angle = GL.GLfloat
data StateChange
    = Move (Distance,Distance)   -- (forward,rightward)
    | Look Angle                 -- look by 'angle' upward
    | Turn Angle                 -- turn by 'angle' rightword
    | Quit

data WorldState = WorldState {
    posX, posY :: Distance,
    angRL, angUD :: Angle,
    isDoomed :: Bool   
}

int :: (Integral a, Num b) => a -> b
int = fromIntegral

toMicroseconds :: Double -> Int
toMicroseconds t = truncate $ 1000000 * t

glVertex3f :: (GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GL.Vertex3 GL.GLfloat
glVertex3f (x,y,z) = GL.Vertex3 x y z

glVector3f :: (GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GL.Vector3 GL.GLfloat
glVector3f (x,y,z) = GL.Vector3 x y z

glColor4f :: (GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat) -> GL.Color4 GL.GLfloat
glColor4f (r,g,b,a) = GL.Color4 r g b a

data Vector3f = Vector3f (GL.GLfloat, GL.GLfloat, GL.GLfloat)

main = do
  True <- GLFW.initialize

  True <- GLFW.openWindow GLFW.defaultDisplayOptions {
    GLFW.displayOptions_numRedBits = 8,
    GLFW.displayOptions_numGreenBits = 8,
    GLFW.displayOptions_numBlueBits = 8,
    GLFW.displayOptions_numDepthBits = 8,
    GLFW.displayOptions_width = 640,
    GLFW.displayOptions_height = 480
  }

  GL.depthFunc $= Just GL.Less
  GLFW.setWindowSizeCallback cbResize

  chan <- newTChanIO :: IO (TChan StateChange)

  GLFW.setKeyCallback (cbKey chan)

  let initworld = WorldState { posX = 0, posY = 0, angRL = pi/4, angUD = 0, isDoomed = False }
  (mainLoop initworld chan) `finally` quit


mainLoop :: WorldState -> TChan StateChange -> IO ()
mainLoop world chan = do
  world' <- handleEvents chan world 
  if isDoomed world' 
  then return ()
  else do
    t0 <- GLFW.getTime
    draw world
    dt <- (t0 + spf -) <$> GLFW.getTime
    when (dt > 0) $ threadDelay (toMicroseconds dt)
    mainLoop world' chan
  where fps = 60
        spf = recip fps

handleEvents chan world = do
  emptyChan <- atomically $ isEmptyTChan chan
  if emptyChan then return world
  else do
    msg <- atomically $ readTChan chan
    return $ case msg of
      Move (fwd,side) -> moveView (fwd,side) world
      Look angUD -> turnViewUD angUD world
      Turn angRL -> turnViewRL angRL world
      Quit -> world { isDoomed = True }


draw world = do
  (w, h) <- GLFW.getWindowDimensions

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.loadIdentity

  let ratio = w / h
  GL.frustum (0.0-ratio) ratio (-1.0) 1.0 0.5 1.5

  let ang = angRL world
      eye = glVertex3f (posX world, posY world, 1.8)
      at = glVertex3f (posX world + cos ang, posY world + sin ang, 1.8 + sin (angUD world))
      up = glVector3f (0, 0, 1)
  GL.lookAt eye at up

  GL.renderPrimitive GL.Quads $ do
      let a = 20.0
      forM_ [(0,0), (a,0), (a,a), (a,a)] $ \(x, y) -> 
          let vtx = glVertex3f (x,y,0)
              col = glColor4f (0,1,0,1)
          in GL.color col >> GL.vertex vtx

  printErrors
  GL.flush
  GLFW.swapBuffers

printErrors = GL.get GL.errors >>= mapM_ print

quit = GLFW.closeWindow >> GLFW.terminate

moveView (fwd,aside) w = 
    w { posX = dX + posX w, posY = dY + posY w }
  where dX = fwd * cos ang - aside * sin ang
        dY = fwd * sin ang + aside * cos ang
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
  

cbKey chan key action = do
  case key of
    GLFW.KeyEsc -> atomically $ writeTChan chan Quit
    GLFW.KeyLeft -> atomically $ writeTChan chan (Turn (-0.02))
    GLFW.KeyRight -> atomically $ writeTChan chan (Turn 0.02)
    _ -> return ()

cbResize w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (int w) (int h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 45 (int w / int h) 1 100
  GL.matrixMode $= GL.Modelview 0

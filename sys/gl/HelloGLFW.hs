import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

int :: (Integral a, Num b) => a -> b
int = fromIntegral

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

    GLFW.setWindowSizeCallback resize

    GL.depthFunc $= Just GL.Less

    mainLoop `finally` quit


mainLoop = do
    now <- GLFW.getTime
    draw now
    
    esc <- GLFW.keyIsPressed GLFW.KeyEsc
    isClosed <- not `fmap` GLFW.windowIsOpen
    unless (isClosed || esc) $ do
      frameLeft <- (spf + now -) `fmap` GLFW.getTime
      when (frameLeft > 0) $
        threadDelay (truncate $ 1000000 * frameLeft)
      mainLoop
  where fps = 60
        spf = recip fps

draw :: Double -> IO ()
draw t = do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    
    GL.loadIdentity
    GL.translate $ GL.Vector3 0 0 (-50 :: GL.GLfloat)
    GL.scale 10 10 (1 :: GL.GLfloat)

    GL.rotate theta axis

    GL.renderPrimitive GL.Quads $ do
        forM_ [(0,0), (1,0), (1,1), (0,1)] $ \(x, y) -> 
            let vtx = GL.Vertex3 (x - 0.5) (y - 0.5) 0 :: GL.Vertex3 GL.GLfloat
                col = GL.Color4 x y 0 1 :: GL.Color4 GL.GLfloat
            in GL.color col >> GL.vertex vtx

    printErrors
    GL.flush
    GLFW.swapBuffers
  where theta = realToFrac t * 60
        axis = GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat

resize w h = do
    GL.viewport $= (GL.Position 0 0, GL.Size (int w) (int h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.perspective 45 (int w / int h) 1 100
    GL.matrixMode $= GL.Modelview 0

quit = GLFW.closeWindow >> GLFW.terminate

printErrors = GL.get GL.errors >>= mapM_ print


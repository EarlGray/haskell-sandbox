module Main where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL (GLfloat)
import Graphics.UI.GLUT (($=))

main :: IO ()
main = do
    (progname, args) <- GLUT.getArgsAndInitialize
    GLUT.initialWindowPosition $= GLUT.Position 400 200
    GLUT.initialWindowSize $= GLUT.Size 600 400

    window <- GLUT.createWindow "HelloGLUT.hs"
    GLUT.initialDisplayMode $= [GLUT.RGBAMode, GLUT.DoubleBuffered]
    
    GLUT.displayCallback $= do
        GL.clearColor $= GL.Color4 0.0 1.0 0.0 1.0
        GL.clear [GL.ColorBuffer]
        GLUT.flush
        
    GLUT.mainLoop

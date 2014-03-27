import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import qualified Graphics.UI.GLUT as GLUT
import Control.Monad

faces :: [[Vertex3 GLfloat]]
faces = [[(v 0), (v 1), (v 2), (v 3)],
         [(v 4), (v 5), (v 6), (v 7)]
        ]

v :: Int -> Vertex3 GLfloat
v i = Vertex3 (if i4 < 2 then 1.0 else -1.0)
              (if (i4 == 0 || i4 == 3) then 1.0 else -1.0)
              (if i < 4 then -1.0 else 1.0)
         where i4 = i `rem` 4

main :: IO ()
main = do
    (exename, _) <- getArgsAndInitialize
    createWindow "HelloCube.hs"
    initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
    displayCallback $= display
    keyboardMouseCallback $= Just keyboard
    motionCallback $= Just motion

    mainLoop

motion :: MotionCallback
motion (Position x y) = do
    print (x, y)

keyboard :: KeyboardMouseCallback
keyboard key state mods pos = 
    case key of
        GLUT.Char 'q' -> do exit
        _ -> return ()

display :: DisplayCallback
display = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    frustum (-1.0) (1.0) (-1.0) 1.0 0.8 10.0
    lookAt (Vertex3 0 0 0 :: Vertex3 GLdouble) (Vertex3 2.0 1.0 3.0 :: Vertex3 GLdouble)  (Vector3 0 0 1 :: Vector3 GLdouble)
    forM faces $ (\[v0, v1, v2, v3] -> do
        renderPrimitive Quads $ do
            color (Color3 0 1 0 :: Color3 GLdouble)
            vertex v0
            vertex v1
            vertex v2
            vertex v3) --}
    renderObject Solid (Teapot 0.5)
    flush

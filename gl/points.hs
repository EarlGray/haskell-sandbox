import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

n = 360 
radius = 0.8 :: GLfloat
myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = map (\k -> (radius * sin(2 * pi * k/n), radius * cos (2 * pi * k/n), 0.0)) [1..n]

main :: IO ()
main = do
    (progname, _) <- getArgsAndInitialize
    createWindow "Hellom points"
    displayCallback $= display
    mainLoop

display = do
    clear [ ColorBuffer ]
    renderPrimitive Points $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    flush

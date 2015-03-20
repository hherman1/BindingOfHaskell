import Graphics.UI.GLUT
import Data.IORef

type Point3 = (GLfloat,GLfloat,GLfloat)
type Point2 = (GLfloat,GLfloat)
type RadiusF = GLfloat
type Vertices = Int

main :: IO ()
main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Hello world"
	object <- newIORef 0.5
	displayCallback $= display object
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (respond object)
	mainLoop

display :: IORef RadiusF -> DisplayCallback
display ioRadius = do
	radius <- get ioRadius
	clear [ ColorBuffer ]
	preservingMatrix $ renderPrimitive Polygon $ 
		mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) $ sphere radius 10	
	print $ sphere radius 10
	flush

sphere :: RadiusF -> Vertices -> [Point3]
sphere r ni = [(r * sin (2*pi*i/n),r * cos (2*pi*i/n),0) | i <- [1..n]]
	where n = fromIntegral ni

reshape :: ReshapeCallback
reshape size = viewport $= (Position 0 0, size) >> postRedisplay Nothing

respond :: IORef RadiusF -> KeyboardMouseCallback
respond ioRadius (MouseButton _) keyState modifiers position = ioRadius $~! (+1) >> postRedisplay Nothing
respond _ _ _ _ _ = return ()

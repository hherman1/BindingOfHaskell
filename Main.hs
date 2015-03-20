{-# LANGUAGE PatternSynonyms #-}
import Graphics.UI.GLFW
import Graphics.UI.GLFW as GLFW
import Graphics.GL
import Data.Maybe (maybe)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

windowHints = [
	WindowHint'Resizable True,
	WindowHint'Visible True
	]

main :: IO ()
main = do
	b <- GLFW.init
	putStrLn $ show b
	window <- setUpWindow
	makeContextCurrent window
	maybe (return ()) windowLoop window
	terminate

setUpWindow :: IO (Maybe Window)
setUpWindow = do
	mapM_ windowHint windowHints
	window <- createWindow 100 100 "GLFW and gl Test" Nothing Nothing
	maybe (return ()) finishSetUp window
	return window
	where
	finishSetUp window = do
		 setMouseButtonCallback window $ Just onMousePress 



onMousePress :: MouseButtonCallback
onMousePress window MouseButton'1 MouseButtonState'Pressed modifiers = setWindowShouldClose window True
onMousePress _ _ _ _ = return ()

windowLoop :: Window -> IO ()
windowLoop window = do
	swapBuffers window
	render window
	pollEvents
	shouldClose <- windowShouldClose window
	if shouldClose then
		return ()
	else
		windowLoop window

render :: Window -> IO ()
render window = do
	(wx,wy) <- getFramebufferSize window
	let (wx', wy') = (fromIntegral wx, fromIntegral wy)
	glViewport 0 0 wx' wy'
	v <- genBuffer $ flatten3 $ circle 0.5 10
	glEnableVertexAttribArray(0)
	glBindBuffer GL_ARRAY_BUFFER v
	glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr
	glDrawArrays GL_LINE_LOOP 0 10
	glDisableVertexAttribArray 0 

genBuffer :: [GLfloat] -> IO (GLuint)
genBuffer vertices = do
	allocaArray 1 $ \arr -> do 
		glGenBuffers 1 arr
		v <- (return . head) =<< peekArray 1 arr
		glBindBuffer GL_ARRAY_BUFFER v
		withArrayLen vertices $ \size arr ->
			glBufferData GL_ARRAY_BUFFER (4 * fromIntegral size) (castPtr arr) GL_STATIC_DRAW
		return v

flatten3 :: [(a,a,a)] -> [a]
flatten3 ((a,b,c):xs) = a:b:c: flatten3 xs
flatten3 [] = []

flatten2 :: [(a,a)] -> [a]
flatten2 ((a,b):xs) = a:b: flatten2 xs
flatten2 [] = []

tri2 :: Floating f => [f]
tri2 = [
	0, 0,
	0, 0.5,
	0.5, 0
	]

triangle :: Floating f => [f]
triangle = [
	0, 0.5,
	0.5, -0.5,
	-0.5, -0.5
	]
square :: Floating f => f -> [(f,f)]
square n = [(n,n),(n,-n),(-n,n),(-n,-n)]

circle :: Floating f => f -> Int -> [(f,f,f)]
circle r n = [(r * sin (2*pi*i'/n'), r * cos (2*pi*i'/n'),0) | i <- [1..n],let i' = fromIntegral i]
	where n' = fromIntegral n

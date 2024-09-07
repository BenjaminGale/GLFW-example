module Main where

import Control.Monad
import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Paths_GLFW_example (getDataFileName)

import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GL
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer as GL
import qualified Graphics.Rendering.OpenGL.GL.VertexSpec as GL
import qualified Graphics.Rendering.OpenGL.GL.BufferObjects as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders.ShaderObjects as GL
import qualified Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects as GL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrays as GL
import qualified Graphics.Rendering.OpenGL.GL.PrimitiveMode as GL
import qualified Graphics.Rendering.OpenGL.GL.VertexArrayObjects as GL
import qualified Graphics.Rendering.OpenGL.GL.Polygons as GL
import qualified Graphics.GL.Types as GL
import qualified Data.ObjectName as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString as BS

vertices :: [GL.GLfloat]
vertices = [0.5, 0.5, 0.0, 0.5, -0.5, 0.0, -0.5, -0.5, 0.0, -0.5,  0.5, 0.0]

indices :: [GL.GLint]
indices = [0, 1, 3, 1, 2, 3]

stride :: GL.GLsizei
stride = fromIntegral (3 * sizeOf (1 :: GL.GLfloat))

resizeViewport :: GLFW.FramebufferSizeCallback
resizeViewport window width height = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

main :: IO ()
main = do
  success <- GLFW.init
  if not success
    then GLFW.terminate
    else do
      
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

      mWindow <- GLFW.createWindow 800 600 "GLFW Example" Nothing Nothing
      maybe GLFW.terminate run mWindow

run :: GLFW.Window -> IO ()
run window = do
  GLFW.makeContextCurrent $ Just window
  GLFW.setFramebufferSizeCallback window (Just resizeViewport)
  resizeViewport window 800 600

  program <- createShaderProgram
  mainLoop window program

  GLFW.terminate

createShaderProgram :: IO GL.Program
createShaderProgram = do
  vertexShader <- GL.createShader GL.VertexShader
  shaderPath <- getDataFileName "app/shader.vert"
  vertexShaderData <- BS.readFile shaderPath
  GL.shaderSourceBS vertexShader $= vertexShaderData
  GL.compileShader vertexShader

  fragmentShader <- GL.createShader GL.FragmentShader
  shaderPath <- getDataFileName "app/shader.frag"
  fragmentShaderData <- BS.readFile shaderPath
  GL.shaderSourceBS fragmentShader $= fragmentShaderData
  GL.compileShader fragmentShader

  program <- GL.createProgram
  GL.attachShader program vertexShader
  GL.attachShader program fragmentShader
  GL.linkProgram program

  GL.deleteObjectNames [vertexShader, fragmentShader]

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao

  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  withArrayLen vertices $ \len ptr ->
    GL.bufferData GL.ArrayBuffer $= (fromIntegral (len * sizeOf (1 :: GL.GLfloat)), ptr , GL.StaticDraw)

  GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float stride nullPtr)
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

  ebo <- GL.genObjectName
  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  withArrayLen indices $ \len ptr ->
    GL.bufferData GL.ElementArrayBuffer $= (fromIntegral (len * sizeOf (1 :: GL.GLint)), ptr , GL.StaticDraw)

  return program

mainLoop :: GLFW.Window -> GL.Program -> IO ()
mainLoop window program = do
  shouldClose <- GLFW.windowShouldClose window
  unless shouldClose $ do
    GL.clearColor $= (GL.Color4 0.2 0.3 0.3 1.0)
    GL.clear [GL.ColorBuffer]

    GL.currentProgram $= Just program
    GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

    GLFW.swapBuffers window
    GLFW.pollEvents
    mainLoop window program

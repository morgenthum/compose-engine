module ComposeEngine.RenderContext where

import           Data.Text
import qualified SDL

type RenderContext = (SDL.Window, SDL.Renderer)

createContext :: Text -> IO RenderContext
createContext windowTitle = do
  let windowSettings = SDL.defaultWindow { SDL.windowMode = SDL.FullscreenDesktop }
  window   <- SDL.createWindow windowTitle windowSettings
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  return (window, renderer)

deleteContext :: RenderContext -> IO ()
deleteContext (window, renderer) = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

module Amoeba.View.Input.InputAccessor where

import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM (atomically)

import Amoeba.Middleware.GLFW.Facade as GLFW
import Amoeba.Middleware.Tracing.Log as Log

-- From here: https://github.com/bsl/GLFW-b-demo/blob/master/src/Main.hs
data Event =
        NoEvent
      | EventClose
      | EventKey !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
      | EventChar !Char
      
      | EventWindowPos       !GLFW.Window !Int !Int
      | EventWindowSize      !GLFW.Window !Int !Int
      | EventWindowRefresh   !GLFW.Window
      | EventWindowFocus     !GLFW.Window !GLFW.FocusState
      | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
      | EventFramebufferSize !GLFW.Window !Int !Int
      | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
      | EventCursorPos       !GLFW.Window !Double !Double
      | EventCursorEnter     !GLFW.Window !GLFW.CursorState
      | EventScroll          !GLFW.Window !Double !Double
  deriving Show

type InputAccessor = TQueue Event

initInputAccessor :: GLFW.Window -> IO InputAccessor
initInputAccessor window = do
    eventsChan <- newTQueueIO :: IO InputAccessor
    Log.info "Events chan created."
    
    GLFW.setWindowCloseCallback window $ Just $ windowCloseCallback eventsChan
    GLFW.setKeyCallback         window $ Just $ keyCallback         eventsChan
    GLFW.setCharCallback        window $ Just $ charCallback        eventsChan
    
    GLFW.setWindowPosCallback       window $ Just $ windowPosCallback       eventsChan
    GLFW.setWindowSizeCallback      window $ Just $ windowSizeCallback      eventsChan
    GLFW.setWindowRefreshCallback   window $ Just $ windowRefreshCallback   eventsChan
    GLFW.setWindowFocusCallback     window $ Just $ windowFocusCallback     eventsChan
    GLFW.setWindowIconifyCallback   window $ Just $ windowIconifyCallback   eventsChan
    GLFW.setFramebufferSizeCallback window $ Just $ framebufferSizeCallback eventsChan
    GLFW.setMouseButtonCallback     window $ Just $ mouseButtonCallback     eventsChan
    GLFW.setCursorPosCallback       window $ Just $ cursorPosCallback       eventsChan
    GLFW.setCursorEnterCallback     window $ Just $ cursorEnterCallback     eventsChan
    GLFW.setScrollCallback          window $ Just $ scrollCallback          eventsChan
    
    Log.info "Callbacks set."
    return eventsChan


windowCloseCallback :: InputAccessor -> GLFW.Window -> IO ()
keyCallback :: InputAccessor -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
charCallback :: InputAccessor -> GLFW.Window -> Char -> IO ()

windowPosCallback       :: InputAccessor -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: InputAccessor -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowRefreshCallback   :: InputAccessor -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: InputAccessor -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: InputAccessor -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: InputAccessor -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: InputAccessor -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: InputAccessor -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: InputAccessor -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: InputAccessor -> GLFW.Window -> Double -> Double                                                 -> IO ()

windowCloseCallback tc _    = atomically $ writeTQueue tc   EventClose
keyCallback tc _ k sc ka mk = atomically $ writeTQueue tc $ EventKey k sc ka mk
charCallback tc _ c         = atomically $ writeTQueue tc $ EventChar c

windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y

tryReadEvent :: InputAccessor -> IO Event
tryReadEvent ia = do
    GLFW.pollEvents
    atomically $ do
        mbE <- tryReadTQueue ia
        maybe (return NoEvent) return mbE



module XDoTool (drag, click, findGameWindowID, focusWindow) where

import Control.Concurrent (threadDelay)
import Control.Monad ( void )
import System.Process (readCreateProcess, proc, CreateProcess(..))

import Geometry.BoardPositions ( Position(..) )


drag :: Position -> Position -> IO ()
drag from to = do
    _ <- mouseMove (x from) (y from)
    holdWhile $ mouseMove (x to) (y to)

-- Delay so as to make sure clicks are registered before moves, and vice-versa
holdWhile :: IO a -> IO ()
holdWhile act = do
    press
    threadDelay 10000
    act
    threadDelay 10000
    void release

click :: IO ()
click = void $ press >> release

press :: IO String
press = xdotool ["mousedown", show 1]

release :: IO String
release = xdotool ["mouseup", show 1]

mouseMove :: Int -> Int -> IO String
mouseMove x y = xdotool ["mousemove", "--sync", show x, show y]

findGameWindowID :: IO String
findGameWindowID = xdotool ["search", "--name", "SHENZHEN I/O"]

focusWindow :: String -> IO String
focusWindow windowID = xdotool ["windowactivate", "--sync",windowID]

xdotool :: [String] -> IO String
xdotool args = readCreateProcess ((proc "xdotool" args) { cwd = Just "/usr/bin"}) ""
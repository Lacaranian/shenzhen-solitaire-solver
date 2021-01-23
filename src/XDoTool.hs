module XDoTool (drag, click, clickAt, findGameWindowID, focusWindow, mouseMove ) where

import Control.Concurrent (threadDelay)
import Control.Monad ( void )
import System.Process (readCreateProcess, proc, CreateProcess(..))

import Geometry.BoardPositions ( Position(..) )


drag :: Position -> Position -> IO ()
drag (Pos fromX fromY) (Pos toX toY) = do
    _ <- mouseMove fromX fromY
    holdWhile $ mouseMove toX toY

-- Delay so as to make sure clicks are registered before moves, and vice-versa
holdWhile :: IO a -> IO ()
holdWhile act = do
    press
    threadDelay 10000
    act
    threadDelay 10000
    void release

clickAt :: Position -> IO ()
clickAt (Pos x y) = mouseMove x y >> click

click :: IO ()
click = press >> void release

press :: IO String
press = xdotool ["mousedown", "1"]

release :: IO String
release = xdotool ["mouseup", "1"]

mouseMove :: Int -> Int -> IO String
mouseMove x y = xdotool ["mousemove", "--sync", show x, show y]

findGameWindowID :: IO String
findGameWindowID = xdotool ["search", "--name", "SHENZHEN I/O"]

focusWindow :: String -> IO String
focusWindow windowID = xdotool ["windowactivate", "--sync", windowID]

xdotool :: [String] -> IO String
xdotool args = readCreateProcess ((proc "xdotool" args) { cwd = Just "/usr/bin"}) ""
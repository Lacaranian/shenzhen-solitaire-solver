module XDoTool (drag, click, clickAt, findGameWindowID, focusWindow, mouseMove ) where


import Control.Concurrent (threadDelay)
import Control.Monad ( void )
import Data.Maybe ( fromJust, isJust )
import System.Process (readCreateProcess, proc, CreateProcess(..))

import Geometry.BoardPositions ( Position(..) )

import Util (headOption)


drag :: Position -> Position -> IO ()
drag (Pos fromX fromY) (Pos toX toY) = do
    _ <- smoothMouseMove 5 fromX fromY
    holdWhile $ smoothMouseMove 20 toX toY

-- Delay so as to make sure clicks are registered before moves, and vice-versa
holdWhile :: IO a -> IO ()
holdWhile act = do
    press
    threadDelay 15000
    act
    threadDelay 15000
    void release

clickAt :: Position -> IO ()
clickAt (Pos x y) = mouseMove x y >> click

click :: IO ()
click = press >> void release

press :: IO String
press = xdotool ["mousedown", "1"]

release :: IO String
release = xdotool ["mouseup", "1"]

-- Moving only a few pixels at a time, lerping
smoothMouseMove :: Int -> Int -> Int -> IO ()
smoothMouseMove granularity x y = do
    Just (curX, curY) <- getMouseLocation 
    let (xDiff, yDiff) = (x - curX, y - curY)
    let stepDestinations = [ (round $ fromIntegral curX + stepX, round $ fromIntegral curY + stepY) 
         | stepNum <- [1..granularity],     
           let stepX = fromIntegral (xDiff * stepNum) / fromIntegral granularity,
           let stepY = fromIntegral (yDiff * stepNum) / fromIntegral granularity ]
    putStrLn $ "Smooth mouse move from " ++ show (curX, curY) ++ " to " ++ show (x, y) ++ " via " ++ show stepDestinations
    mapM_ (uncurry mouseMove) stepDestinations
    

mouseMove :: Int -> Int -> IO String
mouseMove x y = xdotool ["mousemove", "--sync", show x, show y]

findGameWindowID :: IO String
findGameWindowID = xdotool ["search", "--name", "SHENZHEN I/O"]

focusWindow :: String -> IO String
focusWindow windowID = xdotool ["windowactivate", "--sync", windowID]

getMouseLocation :: IO (Maybe (Int, Int))
getMouseLocation = do
    rawStr <- xdotool ["getmouselocation", "--shell"]
    let rawLines = lines rawStr
    let maybeX = fmap (read . drop 2) . headOption $ rawLines
    let maybeY = fmap (read . drop 2) . headOption . drop 1 $ rawLines
    return $ if isJust maybeX && isJust maybeY then Just (fromJust maybeX, fromJust maybeY) else Nothing

xdotool :: [String] -> IO String
xdotool args = readCreateProcess ((proc "xdotool" args) { cwd = Just "/usr/bin"}) ""
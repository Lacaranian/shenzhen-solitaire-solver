module Lib (winGame) where

import Control.Concurrent (threadDelay)
import Control.Monad ( void )

import Geometry.BoardPositions ( Position(Pos) )
import Geometry.BoardRegions ( center )
import Geometry.CardStacks ( cardAtStack, freeCells )

import Game.Actions (availableActions, execWithUpdate)
import Game.State ( Game ) 
import Game.FromScreen (gameFromScreen)

import XDoTool (drag, findGameWindowID, focusWindow)
import ScreenCapture ( cropPixbuf, screenshot )


winGame :: IO ()
winGame = do
    --Initialization
    -- ) Focus the Shenzhen IO window
    windowID <- findGameWindowID
    focusWindow windowID
    -- Start looping
    mainLoop

mainLoop :: IO ()
mainLoop = do
    -- 1) take an in-memory screenshot of the screen
    screenPixBuf <- screenshot
    -- 2) identify all cards/slots/buttons we expect to see (pixels -> board state)
        -- pixbufCopyArea to get subregions to identify
    game <- gameFromScreen screenPixBuf
    print $ show game
    actionLoop game
    --   4a) if no valid moves are possible, start a new game
    print "TODO"
    undefined
    mainLoop

actionLoop :: Game -> IO ()
actionLoop game = do
    -- 3) identify valid moves possible on the current board
    let availActs = availableActions game
    threadDelay 1000000 -- to let the game open before moving things
    case availActs of
        []       -> void $ putStrLn ("Found no actions for game state" ++ show game)
        -- 4) take the best possible action (moves with a mouse click mouse press + drag + release)
        (x : xs) -> putStrLn ("Chosen move: " ++ show x) >> execWithUpdate game x >>= actionLoop

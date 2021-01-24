module Lib (winGame) where

import Control.Concurrent (threadDelay)
import Control.Monad ( void )

import Game.Actions ( Action, availableActions, execWithUpdate)
import Game.AI ( scoredAI )
import Game.State ( Game ) 
import Game.FromScreen (gameFromScreen)

import XDoTool ( findGameWindowID, focusWindow, mouseMove )
import ScreenCapture ( screenshot )

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
    mouseMove 0 0 -- to avoid screenshotting the mouse covering game elements
    game <- gameFromScreen screenPixBuf
    print $ show game
    -- 3) Make good moves as long as possible
    actionLoop game
    -- 4) if no valid moves are possible, start a new game
    print "TODO"
    undefined
    mainLoop

actionLoop :: Game -> IO ()
actionLoop game = do
    -- 1) identify valid moves possible on the current board
    let availActs = availableActions game
    threadDelay 5000000 -- 0.5 seconds, to let the game open/update before moving things along
    let bestAct = scoredAI game availActs
    case bestAct of
        Nothing  -> void $ putStrLn "" >> putStrLn ("Found no actions among " ++ show availActs ++ " for game state:") >> print game
        -- 4) take the best possible action (moves with a mouse click mouse press + drag + release)
        Just act -> takeMove game act

takeMove :: Game -> Action -> IO ()
takeMove game act = do 
    putStrLn ("Chosen move: " ++ show act)
    newGame <- execWithUpdate game act
    actionLoop newGame


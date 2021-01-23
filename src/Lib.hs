module Lib (winGame) where

import Control.Concurrent (threadDelay)
import Control.Monad ( void )

import Geometry.BoardPositions ( Position(Pos) )
import Geometry.BoardRegions ( center )
import Geometry.CardStacks ( cardAtStack, freeCells )

import Game.Actions ( Action, availableActions, execWithUpdate)
import Game.AI ( firstMoveAI, maximizeMovesAI )
import Game.State ( Game ) 
import Game.FromScreen (gameFromScreen)

import XDoTool ( drag, findGameWindowID, focusWindow, mouseMove )
import ScreenCapture ( cropPixbuf, screenshot )
import Util ( firstJust ) 

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
    actionLoop game
    --   4a) if no valid moves are possible, start a new game
    print "TODO"
    undefined
    mainLoop

actionLoop :: Game -> IO ()
actionLoop game = do
    -- 3) identify valid moves possible on the current board
    let availActs = availableActions game
    threadDelay 100000 -- to let the game open before moving things along
    let bestAct = firstJust (map uncurry [maximizeMovesAI, firstMoveAI]) (game, availActs)
    case bestAct of
        Nothing  -> void $ putStrLn "" >> putStrLn ("Found no actions among "++ show availActs ++" for game state" ++ show game)
        -- 4) take the best possible action (moves with a mouse click mouse press + drag + release)
        Just act -> takeMove game act

takeMove :: Game -> Action -> IO ()
takeMove game act = do 
    putStrLn ("Chosen move: " ++ show act)
    threadDelay 1000
    newGame <- execWithUpdate game act
    actionLoop newGame


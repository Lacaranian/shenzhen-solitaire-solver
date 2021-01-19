module Lib (winGame) where

import Control.Concurrent (threadDelay)

import Geometry.BoardPositions ( Position(Pos) )
import Geometry.BoardRegions ( center )
import Geometry.CardStacks ( cardAtStack, freeCells )

import Game.Actions (availableActions, exec)
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
    -- 3) identify valid moves possible on the current board
    let availActs = availableActions game
    print $ show availActs
    -- 4) take the best possible action (moves with a mouse click mouse press + drag + release)
    threadDelay 300000 -- to see the game open before moving things, 5 second delay
    exec $ head availActs
    --   4a) if no valid moves are possible, start a new game
    print "TODO"
    -- 5) return to 1 - maybe cache unmoving parts of the board's position?
    print "TODO"
    mainLoop

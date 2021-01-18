module Lib
    ( winGame
    , testXDoTool
    ) where

import Control.Concurrent (threadDelay)

import Geometry.BoardPositions ( Position(Pos) )
import Geometry.BoardRegions ( center )
import Geometry.CardStacks ( cardAtStack, freeCells )

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
    print "TODO"
    undefined -- TODO
    -- 3) identify valid moves possible on the current board
    print "TODO"
    -- 4) take a move (semi-random to start, with a mouse click mouse press + drag + release)
    threadDelay 500000 -- to see the game open before moving things
    drag (center $ cardAtStack 0 4) (center $ head freeCells)
    undefined -- TODO
    print "TODO"
    --   4a) if no valid moves are possible, start a new game
    print "TODO"
    -- 5) return to 1 - maybe cache unmoving parts of the board's position?
    print "TODO"
    mainLoop

testXDoTool :: IO ()
testXDoTool = drag (Pos 540 30) (Pos 540 500)
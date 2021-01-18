module SymbolRecognition where

import Data.Char (isSpace)
import Data.Maybe ( fromMaybe, catMaybes, listToMaybe ) 
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)
import Control.Monad ( void )
import System.Process (readCreateProcess, proc, CreateProcess(..))

import CropInfo (CropInfo(..), CropIndex(..), CropType(..), savePath)
import Game.State (Card(..), CardSuit(..), DragonSuit(..))
import Geometry.BoardPositions ( Position(..) )

classifyCard :: CropInfo -> IO (Card, String)
classifyCard subscropInfo = do
    (maybeNum, trimStr) <- numOfCard subscropInfo
    let withSuit = fmap (`Card` Stones) maybeNum -- TODO actual suit
    let asCard = listToMaybe . catMaybes $ [withSuit, maybeSpecialCard trimStr] 
    return (fromMaybe Rose asCard, trimStr)

maybeSpecialCard :: String -> Maybe Card 
maybeSpecialCard "ia" = Just $ Dragon White
maybeSpecialCard "tp" = Just $ Dragon Red
maybeSpecialCard "a"  = Just $ Dragon Green
maybeSpecialCard "g"  = Just Rose
maybeSpecialCard _    = Nothing

numOfCard :: CropInfo -> IO (Maybe Int, String)
numOfCard ci = do
    resultStr <- tesseract (savePath ci) "eng"
    return (readMaybe resultStr, trim resultStr)

trim :: [Char] -> [Char]
trim x = takeWhile (not . isSpace) (dropWhile isSpace x)

-- "img/cardStacks/a.png"
tesseract :: String -> String -> IO String
tesseract img lang = readCreateProcess ((proc "tesseract" [img, "stdout","--psm","10", "--dpi", "300", "-l", lang]) { cwd = Just "/usr/bin"}) ""
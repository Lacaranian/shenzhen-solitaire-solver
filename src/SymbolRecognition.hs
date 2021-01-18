module SymbolRecognition where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Char (isSpace)
import Data.Maybe ( fromMaybe, catMaybes, listToMaybe ) 
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)
import Control.Monad ( void )
import System.Process (readCreateProcess, proc, CreateProcess(..))

import CropInfo (CropInfo(..), CropIndex(..), CropType(..), savePath)
import Game.State (Card(..), CardSuit(..), DragonSuit(..))
import Geometry.BoardPositions ( Position(..) )
import Geometry.BoardRegions ( Region(..), center )
import Geometry.CardStacks (cardNumberSize)
import ScreenCapture (pixelPatchTest)

classifyCard :: CropInfo -> IO (Card, String, Maybe String)
classifyCard subscropInfo = do
    (maybeNum, trimStr) <- numOfCard subscropInfo
    (probablyCard, maybeSuitColor) <- case maybeNum of 
        Just num -> suitOfCard subscropInfo <&> bimap (Just . Card num) Just
        Nothing  -> return (maybeSpecialCard trimStr, Nothing)
    return (fromMaybe Rose probablyCard, trimStr, maybeSuitColor) -- If it didn't have a number, and didn't seem like a Dragon/Rose, just default to considering it a rose



numOfCard :: CropInfo -> IO (Maybe Int, String)
numOfCard ci = do
    resultStr <- tesseract (savePath ci) "eng"
    return (readMaybe resultStr, trim resultStr)

-- Just based on what Tesseract consistently categorizes these symbols as, while assuming them to be english letters
maybeSpecialCard :: String -> Maybe Card 
maybeSpecialCard "ia" = Just $ Dragon White
maybeSpecialCard "tp" = Just $ Dragon Red
maybeSpecialCard "a"  = Just $ Dragon Green
maybeSpecialCard "g"  = Just Rose
maybeSpecialCard _    = Nothing

suitOfCard :: CropInfo -> IO (CardSuit, String)
suitOfCard ci = do
    r : g : b : _ <- pixelPatchTest (pixbuf ci) (center cardNumberSize) 4
    let suit
           | r - 20 > g && r - 20 > b = Stones
           | g - 20 > r && g - 20 > b = Sticks
           | otherwise                = Characters
    return (suit, show (r, g, b))

trim :: [Char] -> [Char]
trim x = takeWhile (not . isSpace) (dropWhile isSpace x)

tesseract :: String -> String -> IO String
tesseract img lang = readCreateProcess ((proc "tesseract" [img, "stdout","--psm","10", "--dpi", "300", "-l", lang]) { cwd = Just "/usr/bin"}) ""
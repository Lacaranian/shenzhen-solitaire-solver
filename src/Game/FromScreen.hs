module Game.FromScreen (gameFromScreen, CropInfo(..)) where

import Data.Maybe ( catMaybes, isJust )
import Data.Word (Word8(..))
import System.Directory (getCurrentDirectory)

import Graphics.UI.Gtk (Pixbuf(..))

import CropInfo (CropInfo(..), CropIndex(..), CropType(..))
import Game.State (Game(..), Card(..))
import Geometry.CardStacks (cardNumInfo,  cardAtStack, freeCells, goalCells, cardPresencePosition )
import ScreenCapture ( saveCrop, cropPixbuf, pixelPatchTest )
import SymbolRecognition ( classifyCard )


gameFromScreen :: Pixbuf -> IO Game
gameFromScreen pixbuf = do
    -- crops from the main pixbuf
    freeCellCrops <- mapM (cropPixbuf pixbuf) freeCells
    goalStackCrops <- mapM (cropPixbuf pixbuf) goalCells
    stacksCrops <- cardStacksCrops pixbuf
    cwd <- getCurrentDirectory
    let freeCellCropInfos = zipWith (\idx pb -> CropInfo pb (cwd ++ "/img/freeCells/") (Single idx) CardCrop) [0..] freeCellCrops
    let goalStackCropInfos = zipWith (\idx pb -> CropInfo pb (cwd ++ "/img/goalCells/") (Single idx) CardCrop) [0..] goalStackCrops
    let cardStackCropInfos = zipWith (\stackID stack -> zipWith (\idx pb -> CropInfo pb (cwd ++ "/img/cardStacks/") (Double stackID idx) CardCrop) [0..] stack) [0..] stacksCrops
    mapM_ saveCrop $ freeCellCropInfos ++ goalStackCropInfos ++ concat cardStackCropInfos
    -- cards from those crops
    putStrLn "Free Cells: "
    freeCellCards <- mapM cardFromCardCrop freeCellCropInfos
    putStrLn "Goal stacks: "
    goalStackCards <- mapM cardFromCardCrop goalStackCropInfos
    putStrLn "Card stacks: "
    cardStacks <- mapM cardsForStack cardStackCropInfos
    return $ Game freeCellCards goalStackCards cardStacks

cardsForStack :: [CropInfo] -> IO [Card]
cardsForStack cropInfos = do 
    putStrLn "Next Card stack: "
    maybeCards <- mapM cardFromCardCrop cropInfos 
    return . catMaybes . takeWhile isJust $ maybeCards -- On hitting a blank card, stop there

cardStacksCrops :: Pixbuf -> IO [[Pixbuf]]
cardStacksCrops pixbuf = mapM (cardStackCrops pixbuf) [0..7] 
        
cardStackCrops :: Pixbuf -> Int -> IO [Pixbuf]
cardStackCrops pixbuf stackNum = mapM (cropPixbuf pixbuf . cardAtStack stackNum) [0..12]

-- Check the bottom center of the card to make sure its actually there (either the empty check background or green felt, and its missing)
-- Check the suit/num in the top left of the card to determine which one it is
cardFromCardCrop :: CropInfo -> IO (Maybe Card) 
cardFromCardCrop ci@(CropInfo pixbuf bd idx ct) = do
    (exists, (r, g, b)) <- cardExists pixbuf
    (maybeCard, classif, mSuitColor) <- if exists 
        then cardFromCardInfo ci
        else return (Nothing, "", Nothing)
    putStrLn $ "Exists Color? (" ++ show exists ++ "): " ++ show (r, g, b) ++ " classified as '" ++ classif ++ "' with suit color " ++ show mSuitColor
    return maybeCard 

cardFromCardInfo :: CropInfo -> IO (Maybe Card, String, Maybe String)
cardFromCardInfo ci@(CropInfo pixbuf bd idx ct) = do
    numInfoCrop <- cropPixbuf pixbuf cardNumInfo
    let subscropInfo = ci { pixbuf = numInfoCrop, baseDir = bd ++ "cardInfo/", cropType = CardNumInfoCrop }
    saveCrop subscropInfo 
    (card, classif, suitColor) <- classifyCard subscropInfo
    return (Just card, classif, suitColor)

-- Assumes the pixbuf passed in is cropped to a card's size
cardExists :: Pixbuf -> IO (Bool, (Word8, Word8, Word8)) 
cardExists pixbuf = do
    r : g : b : _ <- pixelPatchTest pixbuf cardPresencePosition 2
    -- Check that this is simply not green, in addition to having a suit/number in the top left
    let nonGreenAvg = ((fromIntegral r :: Double) + (fromIntegral b :: Double)) / 2 
    return (round (2 * nonGreenAvg) >= fromIntegral g, (r, g, b))


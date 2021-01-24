{-# LANGUAGE OverloadedStrings #-}
module ScreenCapture (cropPixbuf, screenshot, pixelTest, pixelPatchTest, saveCrop) where

import Data.Word (Word8(..))

import Data.Array.MArray (readArray)

import Graphics.UI.Gtk
    ( drawWindowGetOrigin,
      drawableGetSize,
      pixbufGetFromDrawable,
      pixbufGetNChannels,
      pixbufGetPixels,
      pixbufGetRowstride,
      pixbufNewSubpixbuf,
      pixbufSave,
      screenGetDefault,
      screenGetRootWindow,
      initGUI,
      Pixbuf,
      PixbufData(..),
      Rectangle(Rectangle))

import CropInfo (CropInfo(..), savePath)
import Geometry.BoardPositions (Position(..))
import Geometry.BoardRegions (Region(..))


screenshot :: IO Pixbuf
screenshot = do
    _ <- initGUI
    Just screen <- screenGetDefault
    window <- screenGetRootWindow screen
    size <- drawableGetSize window
    origin <- drawWindowGetOrigin window
    Just pxbuf <-
        pixbufGetFromDrawable
            window
            ((uncurry . uncurry Rectangle) origin size)
    return pxbuf

saveCrop :: CropInfo -> IO ()
saveCrop ci@(CropInfo pixbuf path idx _) = pixbufSave pixbuf (savePath ci) "png" ([] :: [(String, String)])

-- maybe a good use for pixbufNewSubpixbuf instead?
cropPixbuf :: Pixbuf -> Region -> IO Pixbuf
cropPixbuf src (Reg (Pos x1 y1) (Pos x2 y2))  = do
    pixbufNewSubpixbuf src x1 y1 width height
    where 
        width = x2 - x1
        height = y2 - y1

pixelTest :: Pixbuf -> Position -> IO [Word8]
pixelTest pixbuf pos@(Pos x y) = do
    rowstride <- pixbufGetRowstride pixbuf
    nChannels <- pixbufGetNChannels pixbuf
    let p = informedPixelIndex rowstride nChannels pos
    datum <- (pixbufGetPixels pixbuf :: IO (PixbufData Int Word8))
    mapM (\channel -> readArray datum (p + channel)) [0 .. (nChannels - 1)]

-- Take the average of a target pixel and surrounding pixels
pixelPatchTest :: Pixbuf -> Position -> Int -> IO [Word8]
pixelPatchTest pixbuf pos@(Pos x y) spread = do
    rowstride <- pixbufGetRowstride pixbuf
    nChannels <- pixbufGetNChannels pixbuf
    let baseIndices = map (informedPixelIndex rowstride nChannels) (spreadPositions pos spread)
    datum <- (pixbufGetPixels pixbuf :: IO (PixbufData Int Word8))
    pixelDatas <- mapM (\p -> mapM (\channel -> readArray datum (p + channel)) [0 .. (nChannels - 1)]) baseIndices
    return $ avgPixels nChannels pixelDatas

-- A square of all positions surrounding the starting one
spreadPositions :: Position -> Int -> [Position]
spreadPositions (Pos x y) spread = do
    someX <- [x - spread .. x + spread]
    Pos someX <$> [y - spread .. y + spread]

-- Assumes each sub-array (representing all channels of a single pixel) are the same length
avgPixels :: Int -> [[Word8]] -> [Word8] 
avgPixels nChannels (firstPixel : restPixels) = map round avgPixel
    where (count, avgPixel) = foldl rollingListAvg (1, map fromIntegral firstPixel) $ map (map fromIntegral) restPixels
avgPixels nChannels []                        = undefined -- Also assumes at least one pixel exists in the list
    

rollingListAvg :: (Num a, Real a, Fractional a) => (Int, [a]) -> [a] -> (Int, [a])
rollingListAvg (soFarCount, soFarAvgPixel) pixel = (soFarCount + 1, newAvg)
    where
        newAvg = zipWith zipChannelsAvg soFarAvgPixel pixel
        zipChannelsAvg soFarAvgChannel channel = ((fromIntegral soFarCount * soFarAvgChannel) + channel) / fromIntegral (soFarCount + 1)


-- The index of the pixel in the raw data of a pixbuf
-- Notably, this is an array index at the first channel of the pixel - additional channels are at offsets
informedPixelIndex :: Int -> Int -> Position -> Int
informedPixelIndex rowstride nChannels (Pos x y) = y * rowstride + x * nChannels
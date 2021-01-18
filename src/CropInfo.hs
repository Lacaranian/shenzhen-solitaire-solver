module CropInfo where

import Graphics.UI.Gtk (Pixbuf(..))

data CropInfo = CropInfo { pixbuf :: Pixbuf, baseDir :: FilePath, index :: CropIndex, cropType :: CropType }
data CropIndex = Single Int | Double Int Int
data CropType = CardCrop | CardFullInfoCrop |  CardNumInfoCrop

instance Show CropIndex where
    show (Single idx) = show idx
    show (Double idx1 idx2) = show idx1 ++ "-" ++ show idx2

savePath :: CropInfo -> String 
savePath (CropInfo _ baseDir idx _) = (baseDir ++ show idx ++ ".png")
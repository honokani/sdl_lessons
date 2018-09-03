{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module InfoLoader.PicInfo
    --( getAlphaColor
    --)
    where

import           Foreign.C.Types                   (CInt)
import           Data.ByteString.Lazy.Char8        (pack)
import           Data.Aeson                        (decode, FromJSON,parseJSON)
import           Data.Aeson.TH                     (deriveJSON, defaultOptions, Options(..))
import qualified Data.Text                  as T   (pack, Text)
import qualified System.FilePath.Posix      as SFP

data TipsInfo = TInfo { dots :: DotsInfo
                      } deriving Show
data DotsInfo = DInfo { alpha    :: String
                      , contents :: Contents
                      } deriving Show
data Contents = Cons { red    :: TipStatus
                     , yellow :: TipStatus
                     , green  :: TipStatus
                     , blue   :: TipStatus
                     } deriving Show
data TipStatus = TStat { startX :: Int
                       , startY :: Int
                       , sizeX  :: Int
                       , sizeY  :: Int
                       } deriving Show

deriveJSON defaultOptions ''TipsInfo
--instance FromJSON TipsInfo where
--    parseJSON (Object j) = do
--        dotsO <- j .: "dots"
--        dotsAlpha <- dotsO .: "alpha"
--        return $ TInfo dotsAlpha
deriveJSON defaultOptions ''DotsInfo
deriveJSON defaultOptions ''Contents
deriveJSON defaultOptions ''TipStatus


type LoadTipsInfo = FilePath -> IO (Maybe TipsInfo)
loadTipsInfo :: LoadTipsInfo
loadTipsInfo p = do
    f <- readFile p
    return.decode.pack $ f

getAlphaColor j = case j of
    (Just i) -> alpha.dots $ i
    Nothing  -> ""


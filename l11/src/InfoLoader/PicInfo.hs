{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module InfoLoader.PicInfo
    ( TipsInfo(..)
    , loadTipsInfo
    , getAlphaColor
    )
    where

import           Foreign.C.Types                   (CInt)
import           Data.ByteString.Lazy.Char8        (pack)
import           Data.Aeson                        (decode, FromJSON,parseJSON)
import           Data.Aeson.TH                     (deriveJSON, defaultOptions, Options(..))
import qualified Data.Text                  as T   (pack, Text)
import qualified System.FilePath.Posix      as SFP
-- my module

data TipsInfo = TInfo { dots :: DotsInfo String
                      } deriving Show
data DotsInfo a = DInfo { target  :: a
                        , alpha   :: String
                        , details :: Details
                        } deriving Show
data Details = Det { red    :: TipStatus
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
deriveJSON defaultOptions ''Details
deriveJSON defaultOptions ''TipStatus

loadTipsInfo :: FilePath -> IO (Maybe TipsInfo )
loadTipsInfo p = do
    f <- readFile p
    return.decode.pack $ f

getAlphaColor j = case j of
    (Just i) -> alpha.dots $ i
    Nothing  -> ""


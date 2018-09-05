{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module InfoLoader.WindowInfo
    ( WindowInfo(..)
    , loadWindowInfo
    , restructWindowInfo
    )
    where

import           Foreign.C.Types                   (CInt)
import           Data.ByteString.Lazy.Char8        (pack)
import           Data.Aeson                        (decode)
import           Data.Aeson.TH                     (deriveJSON, defaultOptions, Options(..))
import qualified Data.Text                  as T   (pack, Text)
import qualified System.FilePath.Posix      as SFP
-- my module

data WindowInfo = WinInfo { winTitle :: String
                          , winSizeW :: Int
                          , winSizeH :: Int
                          } deriving Show
deriveJSON defaultOptions ''WindowInfo

loadWindowInfo :: FilePath -> IO (Maybe WindowInfo)
loadWindowInfo p = do
    fWinInfo <- readFile p
    return.decode.pack $ fWinInfo

restructWindowInfo :: WindowInfo -> (T.Text,(CInt,CInt))
restructWindowInfo info = (t,(w,h))
    where
        t = T.pack $ winTitle info
        w = fromIntegral.winSizeW $ info
        h = fromIntegral.winSizeH $ info


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module LoadInfo
    ( loadWindowInfo
    ) where

import           Foreign.C.Types            (CInt)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Aeson                 (decode)
import           Data.Aeson.TH              (deriveJSON, defaultOptions, Options(..))

data WindowInfo = WinInfo { winTitle :: String
                          , winSizeW :: Int
                          , winSizeH :: Int
                          } deriving Show
deriveJSON defaultOptions ''WindowInfo

loadWindowInfo :: FilePath -> IO (Maybe WindowInfo)
loadWindowInfo path = do
    fWinInfo <- readFile path
    return.decode.pack $ fWinInfo

restructWindowInfo :: WindowInfo -> (String,(CInt,CInt))
restructWindowInfo info = (t,(w,h))
    where
        t = winTitle info
        w = fromIntegral.winSizeW $ info
        h = fromIntegral.winSizeH $ info


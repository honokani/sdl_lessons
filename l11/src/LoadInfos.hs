{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module LoadInfos
    ( loadWindowInfo
    , restructWindowInfo
    ) where

import           Foreign.C.Types                   (CInt)
import           Data.ByteString.Lazy.Char8        (pack)
import           Data.Aeson                        (decode)
import           Data.Aeson.TH                     (deriveJSON, defaultOptions, Options(..))
import qualified Data.Text                  as T   (pack, Text)
import qualified System.FilePath.Posix      as SFP

data WindowInfo = WinInfo { winTitle :: String
                          , winSizeW :: Int
                          , winSizeH :: Int
                          } deriving Show
deriveJSON defaultOptions ''WindowInfo


loadWindowInfo :: FilePath -> FilePath -> IO (Maybe WindowInfo)
loadWindowInfo p n = do
    fWinInfo <- readFile $ SFP.joinPath [p,n]
    return.decode.pack $ fWinInfo

restructWindowInfo :: WindowInfo -> (T.Text,(CInt,CInt))
restructWindowInfo info = (t,(w,h))
    where
        t = T.pack $ winTitle info
        w = fromIntegral.winSizeW $ info
        h = fromIntegral.winSizeH $ info


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module LoadPics
--    ( loadTipsInfo
--    , TipsInfo(..)
--    , DotsInfo(..)
--    )
    where

import           Foreign.C.Types            (CInt)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Aeson                 --(decode, FromJSON,parseJSON)
import           Data.Aeson.TH              (deriveJSON, defaultOptions, Options(..))
import qualified Data.Text                  as T (pack, Text)

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


loadTipsInfo :: FilePath -> IO (Maybe TipsInfo)
loadTipsInfo path = do
    f <- readFile path
    return.decode.pack $ f

getAlphaColor j = case j of
    (Just i) -> alpha.dots $ i
    Nothing  -> ""







--    ( loadWindowInfo
--    , restructWindowInfo
--    ) where
--
--import System.Directory (getCurrentDirectory, getDirectoryContents)
--
--data Exts = PNG
--          | JPG
--          | JPEG
--          | GIF
--          | WEBP
--
--findPictures_Png path = pics
--    where
--        pics = do
--            ls <- getDirectoryContents path
--
--findFiles :: FilePath -> Exts -> [FilePath]
--findFiles p e = do
--    


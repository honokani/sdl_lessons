{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module InfoLoader.PicInfo
    ( TipsetsInfo(..)
    , TipsInfo(..)
    , loadTipsInfo
    , getParams
    )
    where

import           Foreign.C.Types                   (CInt)
import           Data.ByteString.Lazy.Char8        (pack)
import           Data.Aeson                        (decode, FromJSON,parseJSON)
import           Data.Aeson.TH                     (deriveJSON, defaultOptions, Options(..))
import qualified Data.Text                  as T   (pack, Text)
import qualified System.FilePath.Posix      as SFP
-- my module

newtype TipsetsInfo = TSInfo { tipsets :: [TipsInfo String]
                             } deriving Show
data TipsInfo a = TInfo { target  :: a
                        , alpha   :: String
                        , details :: Contents Params
                        } deriving Show
data Contents a = Con { red    :: a
                      , yellow :: a
                      , green  :: a
                      , blue   :: a
                      } deriving (Show, Functor, Foldable, Traversable)
data Params = Prm { baseX :: Int
                  , baseY :: Int
                  , sizeX  :: Int
                  , sizeY  :: Int
                  } deriving Show

deriveJSON defaultOptions ''TipsetsInfo
--instance FromJSON TipsetsInfo where
--    parseJSON (Object j) = do
--        dotsO <- j .: "dots"
--        dotsAlpha <- dotsO .: "alpha"
--        return $ TSInfo dotsAlpha
deriveJSON defaultOptions ''TipsInfo
deriveJSON defaultOptions ''Contents
deriveJSON defaultOptions ''Params

loadTipsInfo :: FilePath -> IO (Maybe TipsetsInfo)
loadTipsInfo p = do
    f <- readFile p
    return.decode.pack $ f

getParams (Prm bx by sx sy) = (bx,by,sx,sy)


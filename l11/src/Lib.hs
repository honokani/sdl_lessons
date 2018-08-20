{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson11
    ) where

import           Foreign.C.Types                  (CInt)
-- my module
import qualified LoadInfo                   as LI


-- source file path
windowInfoPath :: String
windowInfoPath = "./ress/window_info.json"


err_WindowInfo_NotFound :: IO ()
err_WindowInfo_NotFound = print "Check json of window infomation."

-- window start.
lesson11 :: IO ()
lesson11 = do
    jWinInfo <- LI.loadWindowInfo windowInfoPath
    case jWinInfo of
        Nothing   -> err_WindowInfo_NotFound
        Just info -> do
            print info
            --SDL_U.begin info sdlAction


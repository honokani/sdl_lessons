{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson11
    ) where

import           Foreign.C.Types                  (CInt)
-- my module
import qualified LoadInfos                  as LI
import qualified ErrorMessages              as EM


-- source file path
windowInfoPath :: String
windowInfoPath = "./ress/window_info.json"

-- window start.
lesson11 :: IO ()
lesson11 = do
    jsonWinInfo <- LI.loadWindowInfo windowInfoPath
    case LI.loadWindowInfo windowInfoPath of
        Nothing   -> EM.putMsg EM.WindowInfo_NotFound
        Just info -> do
            SDL_U.begin (LI.restructWindowInfo info) sdlAction

sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    useRenderer w $ \r -> do
        ts <- SDL_F.loadTexturesWithCKey r cyn sfmap
        SDL_U.runUntil_pushX $ draw r ts
        SDL_F.destroyTextures ts


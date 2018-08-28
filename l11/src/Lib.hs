{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson11
    ) where

import           Foreign.C.Types                  (CInt)
import qualified SDL
-- my module
import qualified SdlUtils        as SDL_U
import qualified SdlUtils_Figure as SDL_F
import qualified LoadInfos                  as LI
import qualified ErrorMessages              as EM


-- source file path
data InfoPaths a = IPs { window  :: a
                       , picTips :: a
                       } deriving (Show)
infoPaths :: InfoPaths String
infoPaths = IPs { window  = "./ress/window_info.json"
                , picTips = "pics_tips_info.json"
                }
--structInfoPath p n = 
--infoPaths = mapM () infoNames
--
--
--data SurfaceMap a = SF_Map { tips     :: a
--                           } deriving (Show, Functor, Foldable, Traversable)
--sfmap :: SurfaceMap FilePath
--sfmap = pure 



-- window start.
lesson11 :: IO ()
lesson11 = do
    jsonWinInfo <- LI.loadWindowInfo $ window infoPaths
    case jsonWinInfo of
        Nothing   -> EM.putMsg EM.WindowInfo_NotFound
        Just info -> do
            SDL_U.begin (LI.restructWindowInfo info) sdlAction

sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    useRenderer w $ \r -> do
        -- ts <- SDL_F.loadTexturesWithCKey r cyn sfmap
        SDL_U.runUntil_X $ draw r --ts
        -- SDL_F.destroyTextures ts
    where
        useRenderer win act = do
            r <- SDL.createRenderer win (-1) rendererConfig
            act r
            SDL.destroyRenderer r
        rendererConfig :: SDL.RendererConfig
        rendererConfig = SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedVSyncRenderer
            , SDL.rendererTargetTexture = False
            }

-- core action
--draw :: SDL.Renderer -> SurfaceMap SDL.Texture -> IO ()
draw :: SDL.Renderer -> IO ()
draw r = do
    SDL.delay 1
    SDL_F.clearCanvas r
    -- start
    SDL_F.setViewport r full
    SDL_F.fillArea r blue full
    -- end
    SDL.present r
    where
        blue = SDL_F.RGBA   0   0 255 255
        full = SDL_F.mkRect 0 0 300 300


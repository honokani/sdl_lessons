{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson11
    ) where

-- common
--import qualified System.FilePath.Posix as SFP
-- for sdl
import           Foreign.C.Types                (CInt)
import qualified SDL
-- my module
import qualified SdlUtils              as SDL_U
import qualified SdlUtils_Figure       as SDL_F
import qualified InfoLoader            as IL
import qualified LoadDirs              as LD
import qualified ErrorMessages         as EM


infoNames :: IL.Infos String
infoNames = IL.IFs { IL.window  = "window_info.json"
                   , IL.picTips = "pics_tips_info.json"
                   }

-- window start.
lesson11 :: IO ()
lesson11 = do
    dirs <-  LD.getCurrDirTree
    infos <- IL.loadInfoAll (LD.res dirs) infoNames
    print $ IL.picTips infos
    case IL.window infos of
        (IL.JW i) -> do
            SDL_U.begin (IL.restructWindowInfo i) (sdlAction infos)
        otherwise -> EM.putMsg EM.WindowInfo_NotFound


sdlAction :: IL.Infos IL.JRecords -> SDL.Window -> IO ()
sdlAction infos w = do
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


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson11
    ) where

-- for sdl
import           Foreign.C.Types                (CInt)
import qualified SDL
-- common
--import qualified System.FilePath.Posix as SFP
-- my module
import qualified SdlUtils              as SDL_U
import qualified SdlUtils_Figure       as SDL_F
import qualified LoadInfos             as LI
import qualified LoadPics              as LP
import qualified LoadDirs              as LD
import qualified ErrorMessages         as EM


type FileName = String
data InfoDatas a = IData { window  :: a
                         , picTips :: a
                         } deriving (Show, Functor)
infoNames :: InfoDatas FileName
infoNames = IData { window  = "window_info.json"
                  , picTips = "./ress/pics_tips_info.json"
                  }


-- window start.
lesson11 :: IO ()
lesson11 = do
    dirs <-  LD.getCurrDirTree
    jsonWinInfo <- LI.loadWindowInfo (LD.res dirs) (window infoNames)
    print jsonWinInfo
    --case jsonWinInfo of
    --    Nothing   -> EM.putMsg EM.WindowInfo_NotFound
    --    Just info -> do
    --        SDL_U.begin (LI.restructWindowInfo info) sdlAction

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


{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson11
    ) where

-- common
--import qualified System.FilePath.Posix as SFP
import           Control.Monad                  (forM)
import           Data.Foldable
import           Data.Traversable
-- for sdl
import           Foreign.C.Types                (CInt)
import qualified SDL
-- my module
import qualified SdlUtils              as SDL_U
import qualified SdlUtils.Figure       as SDL_F
import qualified SdlUtils.Color        as SDL_C
import qualified InfoLoader            as IL
import qualified LoadDirs              as LD
import qualified Lib.ErrorMessages         as EM


infoNames :: IL.Infos String
infoNames = IL.IFs { IL.window  = "window_info.json"
                   , IL.picTips = "pics_tips_info.json"
                   }

-- window start.
lesson11 :: IO ()
lesson11 = do
    infos <- IL.loadInfoAll infoNames =<< LD.getResourceDir
    --extendJRecord infos
    case IL.window infos of
        (IL.JRW i) -> SDL_U.begin (IL.restructWindowInfo i) (sdlAction infos)
        _          -> EM.putMsg EM.WindowInfo_NotFound


sdlAction :: IL.Infos IL.JRecords -> SDL.Window -> IO ()
sdlAction infos = useRenderer actionCore
    where
        useRenderer act win = do
            r <- SDL.createRenderer win (-1) rendererConfig
            act r
            SDL.destroyRenderer r
            where
                rType = SDL.AcceleratedVSyncRenderer
                rendererConfig :: SDL.RendererConfig
                rendererConfig = SDL.RendererConfig { SDL.rendererType = rType
                                                    , SDL.rendererTargetTexture = False
                                                    }
        actionCore r = do
            loadTextures r infos
            --print $ ts
            -- SDL_U.runUntil_X $ draw r --ts
            -- SDL_F.destroyTextures ts

loadTextures r (IL.IFs jw jp) = case jp of
    (IL.JRT pInfo) -> do
        n <- forM (IL.getTipsets pInfo) $ \x -> do
            let tips = fmap (splitToTip (load x).IL.getParams) (IL.getDetails x)
            return $ tips
        return $ n
    where
        load x = SDL_F.loadTextureWithCKey r (cKey x) (path x)
            where
                cKey = SDL_C.get.IL.getAlpha
                path = IL.getTarget
        splitToTip pic (bx,by,sx,sy) = SDL.Rectangle (SDL.P bs) sz
            where
                bs = SDL.V2 bx by
                sz = SDL.V2 sx sy



--draw :: SDL.Renderer -> SurfaceMap SDL.Texture -> IO ()
draw :: SDL.Renderer -> IO ()
draw r = do
    SDL.delay 1
    SDL_F.clearCanvas r
    -- start
    SDL_F.setViewport r full
    SDL_F.fillArea r (SDL_C.get "red") full
    -- end
    SDL.present r
    where
        full = SDL_F.mkRect 0 0 300 300


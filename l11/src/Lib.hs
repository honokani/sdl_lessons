{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson11
    ) where

-- common
import           Control.Monad                  (forM)
import           Data.Foldable
import           Data.Traversable
import qualified System.FilePath.Posix as SFP
-- for sdl
import           Foreign.C.Types                (CInt)
import qualified SDL
-- my module
import qualified SdlUtils              as SDL_U
import qualified SdlUtils.Figure       as SDL_F
import qualified SdlUtils.Color        as SDL_C
import qualified InfoLoader            as IL
import qualified LoadDirs              as LD
import qualified Lib.ErrorMessages     as EM


infoNames :: IL.Infos String
infoNames = IL.IFs { IL.window  = "window_info.json"
                   , IL.picTips = "pics_tips_info.json"
                   }

-- window start.
lesson11 :: IO ()
lesson11 = do
    infos <- IL.loadInfoAll infoNames =<< LD.getResourceDir
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
                rendererConfig = SDL.RendererConfig { SDL.rendererType = rType
                                                    , SDL.rendererTargetTexture = False
                                                    }
        actionCore r = do
            ts <- loadTextures r jp
            SDL_U.runUntil_X $ draw r jw ts
            releaseTextures ts
            where
                (IL.IFs jw jp) = infos

loadTextures r (IL.JRT pInfo) = do
    let x = IL.getDotset pInfo
    tipsPath <- LD.getPictipsDir
    pic <- load tipsPath x
    let tipsRenderer = fmap (splitToTip pic.IL.getParams) (IL.getContents x)
    return $ IL.mkTInfo pic (IL.getAlpha x) tipsRenderer
    where
        load p x = SDL_F.loadTextureWithCKey r (cKey x) (SFP.joinPath [p,path x])
            where
                cKey = SDL_C.get.IL.getAlpha
                path = IL.getTarget
        splitToTip tex (bx,by,sx,sy) = \ren area -> SDL_F.renderTextureTip ren area tex mask
            where
                mask = SDL_F.mkRect bx by sx sy

releaseTextures = SDL.destroyTexture.IL.getTarget


--draw :: SDL.Renderer -> SurfaceMap SDL.Texture -> IO ()
--draw :: SDL.Renderer -> IO ()
draw r (IL.JRW jw) tips = do
    initiarize
    visualizeBackGround
    visualizeDots
    where
        wW = IL.winSzW jw
        wH = IL.winSzH jw
        dotSize = 60
        full = SDL_F.mkRect 0 0 wW wH
        -- acts
        initiarize = do
            SDL.present r
            SDL.delay 1
            SDL_F.clearCanvas r
        visualizeBackGround = do
            SDL_F.setViewport r full
            SDL_F.fillArea r (SDL_C.get "white") full
        visualizeDots = do
            IL.red    tipCons r upL
            IL.green  tipCons r upR
            IL.yellow tipCons r dnL
            IL.blue   tipCons r dnR
            where
                upL = SDL_F.mkRect 0 0 dotSize dotSize
                upR = SDL_F.mkRect (wW - dotSize) 0 dotSize dotSize
                dnL = SDL_F.mkRect 0 (wH - dotSize) dotSize dotSize
                dnR = SDL_F.mkRect (wW - dotSize) (wH - dotSize) dotSize dotSize
                tipCons = IL.getContents tips


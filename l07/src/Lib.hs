{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson07
    ) where

import           SDL                (($=))
import qualified SDL
import qualified SDL.Image as SDL_I

import qualified SdlUtils  as SDL_U


lesson07 :: IO ()
lesson07 = do
    SDL.HintRenderScaleQuality $= SDL.ScaleNearest
    SDL_U.begin ("Lesson07!!", (640, 480)) sdlAction


sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    useRenderer w $ \r -> do
        -- ready modules
        t <- SDL_I.loadTexture r "./pics/rendering.png"
        -- do something
        SDL_U.runUntil_pushX (draw r t)
        -- free sources
        SDL.destroyTexture t


draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw r t = do
    SDL.clear r
    SDL.copy r t Nothing Nothing
    SDL.present r


useRenderer :: SDL.Window -> (SDL.Renderer -> IO a) -> IO ()
useRenderer w action = do
    r <- SDL.createRenderer w (-1) rendererConfig
    action r
    SDL.destroyRenderer r

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False
    }


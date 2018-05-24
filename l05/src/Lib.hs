{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson05
    ) where

import qualified SDL
import qualified SdlUtils            as SDL_U

lesson05 :: IO ()
lesson05 = SDL_U.begin ("Lesson05!!", (240, 320)) sdlAction


sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    -- get sources
    sfc <- SDL.getWindowSurface w
    sfcFormat <- SDL.surfaceFormat sfc

    -- ready modules
    img <- SDL.loadBMP "./pics/stretch.bmp"
    imgFixed <- SDL.convertSurface img sfcFormat

    -- do something
    -- draw w sfc imgFixed
    drawTest w sfc img
    SDL_U.runUntil_pushX (SDL.delay 100)

    -- free sources
    mapM_ SDL.freeSurface [imgFixed, img, sfc]

draw :: SDL.Window -> SDL.Surface -> SDL.Surface -> IO ()
draw w sfc img = do
    SDL.surfaceBlitScaled img Nothing sfc Nothing
    SDL.updateWindowSurface w

drawTest :: SDL.Window -> SDL.Surface -> SDL.Surface -> IO ()
drawTest w sfc img = do
    SDL.surfaceBlitScaled img Nothing sfc Nothing
    SDL.updateWindowSurface w

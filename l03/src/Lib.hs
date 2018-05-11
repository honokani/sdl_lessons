{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson03 
    ) where

import qualified SDL
import qualified SdlUtils            as SDL_U

lesson03 :: IO ()
lesson03 = SDL_U.begin ("Lesson03!!", (320, 240)) sdlAction

sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    -- get screen
    sfc <- SDL.getWindowSurface w
    img <- SDL.loadBMP "./pics/hello_world.bmp"

    -- do something
    SDL_U.renderSurface img sfc w
    SDL_U.runUntil_pushX (SDL.delay 100)

    -- free sources
    SDL.freeSurface img
    SDL.freeSurface sfc


{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson01
    ) where

import qualified SDL

lesson01 :: IO ()
lesson01 = do
    -- create window
    SDL.initialize []
    sz <- pure $ SDL.V2 (fromIntegral 640) (fromIntegral 480)
    w  <- SDL.createWindow "Lesson01!" $ SDL.defaultWindow { SDL.windowInitialSize = sz }


    -- get screen
    screen <- SDL.getWindowSurface w
    -- do something
    SDL.surfaceFillRect screen Nothing $ SDL.V4 0xFF 0xFF 0xFF 0xFF
    SDL.updateWindowSurface w
    SDL.delay 2000
    SDL.surfaceFillRect screen Nothing $ SDL.V4 0x00 0x00 0x00 0x00
    SDL.updateWindowSurface w
    SDL.delay 2000
    SDL.freeSurface screen


    --desrtoy window
    SDL.destroyWindow w
    SDL.quit


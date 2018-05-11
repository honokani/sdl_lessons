{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson02
    ) where


import qualified SDL
import           Data.Text (Text)


lesson02 :: IO ()
lesson02 = sdlBegin ("Lesson02!!", (320, 240)) sdlAction where
    sdlAction :: SDL.Window -> IO ()
    sdlAction = \w -> do
        -- get screen
        screen <- SDL.getWindowSurface w
        -- do something
        image  <- SDL.loadBMP "./pics/hello_world.bmp"
        SDL.surfaceBlit image Nothing screen Nothing
        SDL.updateWindowSurface w
        SDL.delay 2000
        -- free screen
        SDL.freeSurface screen



sdlBegin :: (Integral a) => (Text, (a, a)) -> (SDL.Window -> IO ()) -> IO ()
sdlBegin info action = do
    SDL.initialize []
    sdlWindow info action
    SDL.quit

sdlWindow :: (Integral a) => (Text, (a, a)) -> (SDL.Window -> IO ()) -> IO ()
sdlWindow (title, (x,y)) action= do
    sz <- pure $ SDL.V2 (fromIntegral x) (fromIntegral y)
    w  <- SDL.createWindow title $ SDL.defaultWindow { SDL.windowInitialSize = sz }
    action w
    SDL.destroyWindow w


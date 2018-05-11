{-# LANGUAGE OverloadedStrings #-}

module SdlUtils
    ( begin
    , renderSurface
    , runUntil_pushX
    ) where

import           Control.Monad.Extra          (whileM)
import qualified SDL
import           Data.Text (Text)


--start sdl
begin :: (Integral a) => (Text, (a, a)) -> (SDL.Window -> IO ()) -> IO ()
begin info action = do
    SDL.initialize []
    createWindow info action
    SDL.quit

-- create window
createWindow :: (Integral a) => (Text, (a, a)) -> (SDL.Window -> IO ()) -> IO ()
createWindow (title, (x,y)) action = do
    sz <- pure $ SDL.V2 (fromIntegral x) (fromIntegral y)
    w  <- SDL.createWindow title $ SDL.defaultWindow { SDL.windowInitialSize = sz }
    action w
    SDL.destroyWindow w


-- set image to surface, and render surface on window
renderSurface :: SDL.Surface -> SDL.Surface -> SDL.Window -> IO ()
renderSurface img sfc w = do
    SDL.surfaceBlit img Nothing sfc Nothing
    SDL.updateWindowSurface w


-- Close window control
runUntil_pushX action = whileM $
    checkLiving <$> SDL.pollEvent >>= continueUntilGetFalse action

continueUntilGetFalse :: IO a -> Bool -> IO Bool
continueUntilGetFalse f True = True <$ f
continueUntilGetFalse _ _    = pure False


checkLiving :: Maybe SDL.Event -> Bool
checkLiving Nothing  = True
checkLiving (Just s) = (not.isQuitEvent) s

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _ SDL.QuitEvent) = True
isQuitEvent _                           = False



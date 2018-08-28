module SdlUtils
    ( begin
    , renderSurface
    , runUntil_X
    ) where

import           Control.Monad
import           Control.Monad.Extra (whileM)
import           SDL                 (($=))
import           Data.Text           (Text)
import qualified SDL


-- sdl warning control
data SdlWarn a = SW { scale_linear :: a
                    } deriving (Show)
sdl_warn :: SdlWarn [Char]
sdl_warn = SW { scale_linear = "Warning: Linear texture filtering not enabled!"
              }


-- start sdl
begin :: (Integral a) => (Text, (a, a)) -> (SDL.Window -> IO ()) -> IO ()
begin info action = do
    -- initialize
    SDL.initialize [SDL.InitVideo]
    -- renderer setting
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do
        renderQuality <- SDL.get SDL.HintRenderScaleQuality
        when (renderQuality /= SDL.ScaleLinear) $
            putStrLn $ scale_linear sdl_warn
    -- main
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
renderSurface :: SDL.Window -> SDL.Surface -> SDL.Surface -> IO ()
renderSurface w sfc img = do
    SDL.surfaceBlit img Nothing sfc Nothing
    SDL.updateWindowSurface w


-- Close window control
runUntil_X :: IO () -> IO ()
runUntil_X action = whileM $
    checkLiving <$> SDL.pollEvent >>= continueUntilGetFalse action

continueUntilGetFalse :: IO () -> Bool -> IO Bool
continueUntilGetFalse f True = True <$ f
continueUntilGetFalse _ _    = pure False

checkLiving :: Maybe SDL.Event -> Bool
checkLiving Nothing  = True
checkLiving (Just s) = (not.isQuitEvent) s

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _ SDL.QuitEvent) = True
isQuitEvent _                           = False


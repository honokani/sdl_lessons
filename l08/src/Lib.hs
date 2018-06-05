{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( lesson08
    ) where

import           SDL                      (($=))
import           Foreign.C.Types          (CInt)
import qualified SDL
import qualified SDL.Image       as SDL_I
import qualified Data.Word       as W
import qualified SdlUtils        as SDL_U



lesson08 :: IO ()
lesson08 = do
    SDL.HintRenderScaleQuality $= SDL.ScaleNearest
    SDL_U.begin ("Lesson08!!", (640, 480)) sdlAction

sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    useRenderer w $ \r -> do
        SDL_U.runUntil_pushX $ draw r

useRenderer w action = do
    r <- SDL.createRenderer w (-1) rendererConfig
    action r
    SDL.destroyRenderer r

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False
    }



data Color = RGBA W.Word8 W.Word8 W.Word8 W.Word8
             deriving (Show, Eq)

white :: Color
white = RGBA 255 255 255 255
red   :: Color
red   = RGBA 255   0   0 255
green :: Color
green = RGBA   0 255   0 255
blue  :: Color
blue  = RGBA   0   0 255 255
black :: Color
black = RGBA   0   0   0 255

setColor :: SDL.Renderer -> Color -> IO ()
setColor ren (RGBA r g b a) = SDL.rendererDrawColor ren $= SDL.V4 r g b a


draw :: SDL.Renderer -> IO ()
draw r = do
    SDL.delay 1
    clearCanvas r
    setColor r red >> fillRect r shapeR1
    SDL.present r
    where
        shapeR1 = mkRect 10 50 100 250

clearCanvas :: SDL.Renderer -> IO ()
clearCanvas r = do
    setColor r white
    SDL.clear r

fillRect :: SDL.Renderer -> SDL.Rectangle CInt -> IO ()
fillRect r s = SDL.fillRect r (Just s)

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle (SDL.P $ SDL.V2 x y) $ SDL.V2 w h


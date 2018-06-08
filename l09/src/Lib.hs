{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson09
    ) where

import           Foreign.C.Types          (CInt)
import           SDL                      (($=))
import qualified SDL
import qualified SDL.Image       as SDL_I
import qualified Data.Word       as W
import           Data.Text                (Text)
-- my module
import qualified SdlUtils        as SDL_U


-- setting to edit.
windowW, windowH :: CInt
(windowW, windowH) = (640, 480)
windowT :: Text
windowT = "Lesson09!!"
-- fix structure
windowSz :: (CInt, CInt)
windowSz = (windowW, windowH)
windowSetting :: (Text, (CInt, CInt))
windowSetting = (windowT, windowSz)

-- Image containers
data SurfaceMap a = Smap { hello   :: a
                         } deriving (Show, Functor, Foldable, Traversable)
smap :: SurfaceMap FilePath
smap = Smap { hello   = "./pics/hello_world.png"
            }


-- window start.
lesson09 :: IO ()
lesson09 = do
    SDL_U.begin windowSetting sdlAction

sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    useRenderer w $ \r -> do
        t <- SDL_I.loadTexture r $ hello smap
        SDL_U.runUntil_pushX $ draw r t
        SDL.destroyTexture t


-- renderer config AND set use functions
useRenderer w action = do
    r <- SDL.createRenderer w (-1) rendererConfig
    action r
    SDL.destroyRenderer r

rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False
    }


-- * Core Functions *
-- color control
data Color = RGBA W.Word8 W.Word8 W.Word8 W.Word8
             deriving (Show, Eq)
setColor :: SDL.Renderer -> Color -> IO ()
setColor ren (RGBA r g b a) = SDL.rendererDrawColor ren $= SDL.V4 r g b a
wht :: Color
wht = RGBA 255 255 255 255
red :: Color
red = RGBA 255   0   0 255
grn :: Color
grn = RGBA   0 255   0 255
blu :: Color
blu = RGBA   0   0 255 255
blc :: Color
blc = RGBA   0   0   0 255

-- canvas control
clearCanvas :: SDL.Renderer -> IO ()
clearCanvas r = do
    setColor r wht
    SDL.clear r

fillRect :: SDL.Renderer -> SDL.Rectangle CInt -> IO ()
fillRect r s = SDL.fillRect r $ Just s

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle (SDL.P $ SDL.V2 x y) $ SDL.V2 w h

setViewport :: SDL.Renderer ->  SDL.Rectangle CInt -> IO ()
setViewport r sc = SDL.rendererViewport r $= Just sc

pasteTexture :: SDL.Renderer -> SDL.Texture -> IO ()
pasteTexture r t = SDL.copy r t Nothing Nothing

-- core action
draw :: SDL.Renderer -> SDL.Texture -> IO ()
draw r t = do
    SDL.delay 1
    clearCanvas r
    -- topLeft and topRight
    setViewport r full
    setColor r red >> fillRect r topL
    setColor r blu >> fillRect r topR
    -- bottom
    setViewport r bot
    pasteTexture r t
    -- end
    SDL.present r
    where
        winMidH = div windowH 2
        winMidW = div windowW 2
        full = mkRect 0 0 windowW windowH
        topL = mkRect 0 0 winMidW winMidH
        topR = mkRect winMidW 0 winMidW winMidH
        bot  = mkRect 0 winMidH windowW winMidH


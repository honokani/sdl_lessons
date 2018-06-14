{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson10
    ) where

import           Foreign.C.Types          (CInt)
import           SDL                      (($=))
import           Data.Text                (Text)
import qualified SDL
import qualified SDL.Image       as SDL_I
import qualified Data.Word       as W
-- my module
import qualified SdlUtils        as SDL_U
import qualified SdlUtils_Figure as SDL_F


-- setting to edit.
windowW, windowH :: CInt
(windowW, windowH) = (640, 480)
windowT :: Text
windowT = "Lesson10!!"
-- fix structure
windowSz :: (CInt, CInt)
windowSz = (windowW, windowH)
windowSetting :: (Text, (CInt, CInt))
windowSetting = (windowT, windowSz)

-- Image containers
data SurfaceMap a = SFMap { clear08 :: a
                          , cyan24  :: a
                          , clear32  :: a
                          } deriving (Show, Functor, Foldable, Traversable)
sfmap :: SurfaceMap FilePath
sfmap = SFMap { clear08 = "./pics/clear_08.png"
              , cyan24  = "./pics/cyan_24.png"
              , clear32 = "./pics/clear_32.png"
              }


-- window start.
lesson10 :: IO ()
lesson10 = do
    SDL_U.begin windowSetting sdlAction

sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    useRenderer w $ \r -> do
        --t <- SDL_I.loadTexture r $ cyan24 sfMap
        ts <- SDL_F.loadTextures r sfmap
        SDL_U.runUntil_pushX $ draw r ts
        SDL_F.destroyTextures ts

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

wht :: SDL_F.Color
wht = SDL_F.RGBA 255 255 255 255
red :: SDL_F.Color
red = SDL_F.RGBA 255   0   0 255
grn :: SDL_F.Color
grn = SDL_F.RGBA   0 255   0 255
blu :: SDL_F.Color
blu = SDL_F.RGBA   0   0 255 255
blc :: SDL_F.Color
blc = SDL_F.RGBA   0   0   0 255


-- core action
draw :: SDL.Renderer -> SurfaceMap SDL.Texture -> IO ()
draw r ts = do
    SDL.delay 1
    SDL_F.clearCanvas r

    -- topLeft and topRight
    SDL_F.setViewport r full
    SDL_F.fillArea r red topL
    SDL_F.fillArea r blu topR
    -- bottom
    SDL_F.setViewport r bot
    SDL_F.pasteTexture r $ cyan24 ts

    -- end
    SDL.present r
    where
        winMidH = div windowH 2
        winMidW = div windowW 2
        full = SDL_F.mkRect 0 0 windowW windowH
        topL = SDL_F.mkRect 0 0 winMidW winMidH
        topR = SDL_F.mkRect winMidW 0 winMidW winMidH
        bot  = SDL_F.mkRect 0 winMidH windowW winMidH


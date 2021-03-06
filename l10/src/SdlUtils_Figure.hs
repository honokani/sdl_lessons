module SdlUtils_Figure
    ( clearCanvas
    , fillRect
    , mkRect
    , setViewport
    , pasteTexture
    , fillArea
    , loadTextures
    , destroyTextures
    , loadTextureWithCKey
    , loadTexturesWithCKey
    , Color(..)
    ) where

import           Foreign.C.Types          (CInt)
import           SDL                      (($=))
import qualified SDL
import qualified SDL.Image       as SDL_I
import qualified Data.Word       as W

-- color control
data Color = RGBA W.Word8 W.Word8 W.Word8 W.Word8
             deriving (Show, Eq)
color2V4 (RGBA r g b a) = SDL.V4 r g b a
setColor :: SDL.Renderer -> Color -> IO ()
setColor ren c = SDL.rendererDrawColor ren $= color2V4 c

-- canvas control
clearCanvas :: SDL.Renderer -> IO ()
clearCanvas r = do
    setColor r $ RGBA 255 255 255 255
    SDL.clear r

fillRect :: SDL.Renderer -> SDL.Rectangle CInt -> IO ()
fillRect r s = SDL.fillRect r $ Just s

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle (SDL.P $ SDL.V2 x y) $ SDL.V2 w h

setViewport :: SDL.Renderer ->  SDL.Rectangle CInt -> IO ()
setViewport r sc = SDL.rendererViewport r $= Just sc

pasteTexture :: SDL.Renderer -> SDL.Texture -> IO ()
pasteTexture r t = SDL.copy r t Nothing Nothing

fillArea :: SDL.Renderer -> Color -> SDL.Rectangle CInt -> IO ()
fillArea r col area = do
    setColor r col >> fillRect r area

loadTextures :: (Traversable m) => SDL.Renderer -> m FilePath -> IO (m SDL.Texture)
loadTextures r = mapM (SDL_I.loadTexture r)

destroyTextures :: (Traversable m) => m SDL.Texture -> IO ()
destroyTextures = mapM_ SDL.destroyTexture

loadSurfaces :: (Traversable m) => m FilePath -> IO (m SDL.Surface)
loadSurfaces = mapM SDL_I.load


loadTextureWithCKey :: SDL.Renderer -> Color -> FilePath -> IO (SDL.Texture)
loadTextureWithCKey r key fp = do
    sfc <- SDL_I.load fp
    SDL.surfaceColorKey sfc $= (Just $ color2V4 key)
    SDL.createTextureFromSurface r sfc

loadTexturesWithCKey :: (Traversable m) => SDL.Renderer -> Color -> m FilePath -> IO (m SDL.Texture)
loadTexturesWithCKey r k fps = do
    mapM (loadTextureWithCKey r k) fps



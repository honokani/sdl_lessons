module SdlUtils.Figure
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
    , renderTextureTip
    ) where

-- common
import qualified Data.Word       as W
-- for sdl
import           Foreign.C.Types          (CInt)
import           SDL                      (($=))
import qualified SDL
import qualified SDL.Image       as SDL_I
-- my module
import qualified SdlUtils.Color  as SDL_C


-- canvas control
clearCanvas :: SDL.Renderer -> IO ()
clearCanvas r = do
    SDL_C.setColorK r SDL_C.White
    SDL.clear r

fillRect :: SDL.Renderer -> SDL.Rectangle CInt -> IO ()
fillRect r s = SDL.fillRect r $ Just s

mkRect :: (Integral a) => a -> a -> a -> a -> SDL.Rectangle CInt
mkRect x y w h = fromIntegral <$> SDL.Rectangle basePos tSize
    where
        basePos = SDL.P $ SDL.V2 x y
        tSize = SDL.V2 w h

setViewport :: SDL.Renderer ->  SDL.Rectangle CInt -> IO ()
setViewport r sc = SDL.rendererViewport r $= Just sc

pasteTexture :: SDL.Renderer -> SDL.Texture -> IO ()
pasteTexture r t = SDL.copy r t Nothing Nothing

fillArea :: SDL.Renderer -> SDL_C.Color -> SDL.Rectangle CInt -> IO ()
fillArea r col area = do
    SDL_C.setColor r col >> fillRect r area

loadTextures :: (Traversable m) => SDL.Renderer -> m FilePath -> IO (m SDL.Texture)
loadTextures r = mapM (SDL_I.loadTexture r)

destroyTextures :: (Traversable m) => m SDL.Texture -> IO ()
destroyTextures = mapM_ SDL.destroyTexture

loadSurfaces :: (Traversable m) => m FilePath -> IO (m SDL.Surface)
loadSurfaces = mapM SDL_I.load

loadTextureWithCKey :: SDL.Renderer -> SDL_C.Color -> FilePath -> IO (SDL.Texture)
loadTextureWithCKey r key fp = do
    sfc <- SDL_I.load fp
    SDL.surfaceColorKey sfc $= (Just $ SDL_C.color2V4 key)
    SDL.createTextureFromSurface r sfc

loadTexturesWithCKey :: (Traversable m) => SDL.Renderer -> SDL_C.Color -> m FilePath -> IO (m SDL.Texture)
loadTexturesWithCKey r k fps = mapM (loadTextureWithCKey r k) fps

renderTextureTip :: SDL.Renderer -> SDL.Rectangle CInt -> SDL.Texture -> SDL.Rectangle CInt -> IO ()
renderTextureTip ren area tex mask = SDL.copy ren tex (Just mask) (Just area)


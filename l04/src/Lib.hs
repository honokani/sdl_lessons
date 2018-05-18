{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lib
    ( lesson04
    ) where

import           Control.Monad.Extra          (whileM)
import qualified SDL
import qualified SdlUtils            as SDL_U

-- Image containers
data SurfaceMap a = Smap { def   :: a
                         , up    :: a
                         , right :: a
                         , down  :: a
                         , left  :: a
                         , undef :: a
                         } deriving (Show, Functor, Foldable, Traversable)

smap :: SurfaceMap FilePath
smap = Smap { def   = "./pics/hello_world.bmp"
            , up    = "./pics/u.bmp"
            , right = "./pics/r.bmp"
            , down  = "./pics/d.bmp"
            , left  = "./pics/l.bmp"
            , undef = "./pics/hello_world.bmp"
            }


-- main
lesson04 :: IO ()
lesson04 = SDL_U.begin ("Lesson04!!", (320, 240)) sdlAction



sdlAction :: SDL.Window -> IO ()
sdlAction w = do
    -- get sources
    sfc <- SDL.getWindowSurface w
    surfaces <- mapM SDL.loadBMP smap
    -- ready modules
    let doRender = SDL_U.renderSurface w sfc
    let coreAction = do
            msg <- scanEvent <$> SDL.pollEvent
            updateSurface surfaces doRender msg
    -- do something
    doRender $ def surfaces
    whileM coreAction
    -- free sources
    mapM_ SDL.freeSurface surfaces
    SDL.freeSurface sfc


-- key input actions
data TargetKey = UndefKey
               | Up
               | Rt
               | Dn
               | Lt

data Message = UndefMsg
             | Select TargetKey
             | Idle
             | Quit

scanEvent :: Maybe SDL.Event -> Message
scanEvent me = case me of
    Nothing  -> Idle
    (Just e) -> payloadToMessage $ eventToPayload e

eventToPayload :: SDL.Event -> SDL.EventPayload
eventToPayload (SDL.Event _t p) = p

payloadToMessage :: SDL.EventPayload -> Message
payloadToMessage SDL.QuitEvent         = Quit
payloadToMessage (SDL.KeyboardEvent k) = getMessage k
payloadToMessage _                     = Idle

getMessage :: SDL.KeyboardEventData -> Message
getMessage (SDL.KeyboardEventData _ SDL.Released _     _) = Idle
getMessage (SDL.KeyboardEventData _ SDL.Pressed  True  _) = Idle
getMessage (SDL.KeyboardEventData _ SDL.Pressed  False k) =
    case SDL.keysymKeycode k of
        SDL.KeycodeEscape -> Quit
        SDL.KeycodeUp     -> Select Up
        SDL.KeycodeRight  -> Select Rt
        SDL.KeycodeDown   -> Select Dn
        SDL.KeycodeLeft   -> Select Lt
        _                 -> UndefMsg

updateSurface :: SurfaceMap a -> (a -> IO ()) -> Message -> IO Bool
updateSurface ss f msg = case msg of
    Quit     -> pure False
    UndefMsg -> pure True
    Idle     -> pure True
    Select k -> True <$ f (selectSurface k ss)

selectSurface :: TargetKey -> SurfaceMap a -> a
selectSurface UndefKey = undef
selectSurface Up       = up
selectSurface Dn       = down
selectSurface Lt       = left
selectSurface Rt       = right


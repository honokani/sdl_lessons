module SdlUtils.Color
    --( clearCanvas
    --, Color(..)
    --)
    where

-- common
import           Control.Arrow           ((***))
import qualified Data.Word         as W
import qualified Data.Map          as M
-- for sdl
import           SDL                     (($=))
import qualified SDL
import qualified Lib.ErrorMessages as EM


data Color = RGBA W.Word8 W.Word8 W.Word8 W.Word8
           | RGB  W.Word8 W.Word8 W.Word8
           deriving (Show, Eq)

data ColorKind = Black | Navy | Blue
               -- 010
               | Green | Teal | DogerBlue
               -- 020
               | Lime | SpringGreen | Cyan
               -- 100
               | Maroon | Purple | Violet
               -- 110
               | Olive | Gray | SlateBlue
               -- 120
               | Chartreuse | Mint | LightBlue
               -- 200
               | Red | DeepPink | Magenta
               -- 210
               | Orange | LightCoral | Pink
               -- 220
               | Yellow | WitchHaze | White
               deriving (Show, Ord, Eq, Enum)

nameColorMap = M.fromList [ ("black", Black)
                          , ("blue" , Blue)
                          , ("lime" , Lime)
                          , ("red"  , Red)
                          , ("cyan" , Cyan)
                          , ("white", White)
                          ]

get :: String -> Color
get n = kindToColor $ case M.lookup n nameColorMap of
    Nothing  -> White
    (Just c) -> c

kindToColor :: ColorKind -> Color
kindToColor = num27ToCol.fromEnum
    where
        num27ToCol :: Int -> Color
        num27ToCol n = RGB r g b
            where
                fix = fromIntegral.(\x -> if x/=0 then x-1 else x).(*128)
                (r,m) = fix *** id  $ n `divMod` 9
                (g,b) = fix *** fix $ m `divMod` 3

color2V4 (RGBA r g b a) = SDL.V4 r g b a
color2V4 (RGB r g b)    = SDL.V4 r g b 255

colorTransparency n (RGBA r g b _) = SDL.V4 r g b n
colorTransparency n (RGB r g b)    = SDL.V4 r g b n

setColor :: SDL.Renderer -> Color -> IO ()
setColor ren c = SDL.rendererDrawColor ren $= color2V4 c

setColorK :: SDL.Renderer -> ColorKind -> IO ()
setColorK ren = setColor ren.kindToColor


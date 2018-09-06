{-# LANGUAGE OverloadedStrings #-}

module Lib.ErrorMessages
    ( putMsg
    , Msgs(..)
    ) where

-- common
import qualified System.Exit as EX (die)

data Msgs = WindowInfo_NotFound
          | WindowInfo_Lacking
          | SdlUtilColor_NotFound

putMsg :: Msgs -> IO ()
putMsg p = case p of
    WindowInfo_NotFound   -> EX.die "Check json file for window infomation."
    WindowInfo_Lacking    -> EX.die "Window infomation file is lacking."
    SdlUtilColor_NotFound -> EX.die "Color Not Found."


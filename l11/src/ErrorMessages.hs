{-# LANGUAGE OverloadedStrings #-}

module ErrorMessages
    ( putMsg
    , Msgs(..)
    ) where

data Msgs = WindowInfo_NotFound
          | WindowInfo_Lacking

putMsg :: Msgs -> IO ()
putMsg p = case p of
    WindowInfo_NotFound -> print "Check json file for window infomation."
    WindowInfo_Lacking  -> print "Window infomation file is lacking."


{-# LANGUAGE OverloadedStrings #-}

module LoadPics
    ( loadWindowInfo
    , restructWindowInfo
    ) where

import System.Directory (getCurrentDirectory, getDirectoryContents)

data Exts = PNG
          | JPG
          | JPEG
          | GIF
          | WEBP

findPictures_Png path = pics
    where
        pics = do
            ls <- getDirectoryContents path

findFiles :: FilePath -> Exts -> [FilePath]
findFiles p e = do
    


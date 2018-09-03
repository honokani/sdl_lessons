{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

module LoadDirs
    --( getCurrDirPath
    --, getDirTree
    --, getCurrDirTree
    --)
    where

import qualified System.Directory      as SD
import qualified System.FilePath.Posix as SFP

-- user setting ---------------

data DirStructure a = DStruct { _app :: a
                              , res  :: a
                              , pics :: a
                              , tips :: a
                              } deriving (Show, Functor)

dirs :: FilePath -> DirStructure DirMapping
dirs p = DStruct { _app = Rt p
                 , res  = Br "res"  $ self _app
                 , pics = Br "pics" $ self _app
                 , tips = Br "tips" $ self pics
                 }
    where self f = f $ dirs p

--------------------------------

data DirMapping = Rt FilePath
                | Br FilePath DirMapping
                deriving (Show, Eq)

createDirPath :: DirMapping -> FilePath
createDirPath = SFP.joinPath.createDirPathCore
    where
        createDirPathCore :: DirMapping -> [FilePath]
        createDirPathCore (Rt p)   = [p]
        createDirPathCore (Br p u) = createDirPathCore u ++ [p]

getDirTree :: FilePath -> DirStructure FilePath
getDirTree here = fmap createDirPath $ dirs here


getCurrDirPath :: IO FilePath
getCurrDirPath = SD.getCurrentDirectory

getCurrDirTree :: IO (DirStructure FilePath)
getCurrDirTree = getDirTree <$> getCurrDirPath



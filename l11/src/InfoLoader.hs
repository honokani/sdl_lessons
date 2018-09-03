{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}

module InfoLoader
    --( lesson11
    --)
    where

--import           Data.Traversable
-- my module
import qualified LoadDirs              as LD
import qualified InfoLoader.WindowInfo as IW
import qualified InfoLoader.PicInfo    as IP
import qualified System.FilePath.Posix      as SFP

data Infos a = IData { window  :: a
                     , picTips :: a
                     } deriving (Show, Functor, Foldable, Traversable)

data InfoTypes = WinInfo { winTitle :: String
                         , winSizeW :: Int
                         , winSizeH :: Int
                         } deriving Show


--type FileName = String
--load :: FilePath -> IData FileName -> IData 
--load p = mapM $ IW.loadWindowInfo p

zipWithTF f t1 t2 = traverse (\x -> traverse (\y -> uncurry f (x,y)) t2 ) t1
zipTF t1 t2 = zipWithTF (\x y -> (x,y))



iN = IData { window  = "window_info.json"
           , picTips = "pics_tips_info_1.json"
           }
data LoadKind = KW IW.LoadWindowInfo
              | KP IP.LoadTipsInfo
--fN = IData { window  = IW.loadWindowInfo
--           , picTips = IP.loadTipsInfo
--           }

fN_ :: Infos (FilePath -> IO JsonRecord)
fN_ = IData { window  = lowin
            , picTips = lotip
            }
lowin p = do
    t <- IW.loadWindowInfo p
    case t of
        Nothing  -> return Faild
        (Just i) -> return (JW i)

lotip p = do
    t <- IP.loadTipsInfo p
    case t of
        Nothing  -> return Faild
        (Just i) -> return (JT i)


--loadInfoAll dirp fnames = zipWithTF run fN ps
--    where
--        ps = fmap (\x -> SFP.joinPath [dirp,x]) fnames
--        run loader p = case loader of
--            KW f -> f p
--            KP f -> f p

data JsonRecord = JW IW.WindowInfo
                | JT IP.TipsInfo
                | Faild



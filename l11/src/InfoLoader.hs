{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module InfoLoader
    --( lesson11
    --)
    where
-- common
import           Prelude               hiding (sequence)
import           Data.Foldable
import           Data.Traversable
import qualified System.FilePath.Posix as SFP
-- my module
import qualified InfoLoader.PicInfo    as IP
import qualified InfoLoader.WindowInfo as IW

data Infos a = IFs { window  :: a
                   , picTips :: a
                   } deriving (Show, Functor, Foldable, Traversable)

data JRecords = Faild
                 | JT IP.TipsInfo
                 | JW IW.WindowInfo
                 deriving (Show)

class RecordContainer a where
    conv :: a -> JRecords
    get :: JRecords -> a
instance RecordContainer IW.WindowInfo where
    conv = JW
    get jr = case jr of (JW x) -> x
instance RecordContainer IP.TipsInfo where
    conv = JT
    get jr = case jr of (JT x) -> x

getWin :: Infos JRecords -> IW.WindowInfo
getWin = get.window

infoLoaders :: Infos (FilePath -> IO JRecords)
infoLoaders = IFs { window  = setLoader $ IW.loadWindowInfo
                  , picTips = setLoader $ IP.loadTipsInfo
                  }
    where
        setLoader tgt = \p -> do
            t <- tgt p
            case t of
                Nothing  -> return Faild
                (Just i) -> return $ conv i

-------------------------------------------------------

type FileName = String
loadInfoAll :: FilePath -> Infos FileName -> IO (Infos JRecords)
loadInfoAll dirp fnames = zipWithTFM run infoLoaders ps
    where
        ps :: Infos FilePath
        ps = fmap (\x -> SFP.joinPath [dirp,x]) fnames
        run :: (FilePath -> IO JRecords) -> FilePath -> IO JRecords
        run loader p = loader p

restructWindowInfo = IW.restructWindowInfo 





-----------------------------------------------------------------------
-- Sourced By https://wiki.haskell.org/Foldable_and_Traversable
data Supply s v = Supply { unSupply :: [s] -> ([s],v) }

instance Functor (Supply s) where 
    fmap f av = Supply (\l -> let (l',v) = unSupply av l in (l',f v))

instance Applicative (Supply s) where
    pure v    = Supply (\l -> (l,v))
    af <*> av = Supply (\l -> let (l',f)  = unSupply af l
                                  (l'',v) = unSupply av l'
                              in (l'',f v))

runSupply :: (Supply s v) -> [s] -> v
runSupply av l = snd $ unSupply av l

supply :: Supply s s
supply = Supply (\(x:xs) -> (xs,x))

zipTF :: (Traversable t, Foldable f) => t a -> f b -> t (a,b)
zipTF t f = runSupply (traverse (\a -> (,) a <$> supply) t) (toList f)

zipWithTF :: (Traversable t,Foldable f) => (a -> b -> c) -> t a -> f b -> t c
zipWithTF g t f = runSupply  (traverse (\a -> g a <$> supply) t) (toList f)

zipWithTFM :: (Traversable t,Foldable f,Monad m) => (a -> b -> m c) -> t a -> f b -> m (t c)
zipWithTFM g t f = sequence (zipWithTF g t f)


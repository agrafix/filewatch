module System.FileWatch
       ( -- * FileObserver
         loadFileAndObserve, getObservedContent, stopObserving
       , ObservedFile
         -- * Managers
       , withManager, WatchManager, startManager, stopManager
       )
where

import Control.Concurrent.STM
import Control.Monad
import System.FSNotify
import System.FilePath
import qualified Data.ByteString.Lazy as BSL

data ObservedFile a
   = ObservedFile
   { of_contents :: TVar (Maybe a)
   , of_kill :: StopListening
   }

loadFileAndObserve :: WatchManager -> FilePath -> (BSL.ByteString -> Maybe a) -> IO (ObservedFile a)
loadFileAndObserve wm fp parser =
  do ctRef <- newTVarIO Nothing
     let dir = takeDirectory fp
         updateContent = BSL.readFile fp >>= atomically . writeTVar ctRef . parser
     quit <- watchDir wm dir (const True) $ \ev ->
       when (takeFileName (eventPath ev) == takeFileName fp) $
       case ev of
         Added _ _ -> updateContent
         Modified _ _ -> updateContent
         Removed _ _ -> atomically $ writeTVar ctRef Nothing
     return $ ObservedFile ctRef quit

getObservedContent :: ObservedFile a -> IO a
getObservedContent f =
  atomically $
  do val <- readTVar (of_contents f)
     case val of
       Nothing -> retry
       Just x -> return x

stopObserving :: ObservedFile a -> IO ()
stopObserving = of_kill

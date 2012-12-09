-- Imports --
import Prelude hiding (catch)
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.List
import Data.String.Utils
import System.FilePath
import System.Directory
import System.Environment
import Network.Curl.Download
import System.Process
import System.IO
import Text.Feed.Types
import Text.Feed.Query
import Text.XML.Light.Types
import Text.XML.Light.Proc
import Control.Concurrent
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Control.Monad.Parallel as MP

-- Globals --
feedFileName = "feed.url"

-- Types --
type FeedDir = FilePath

-- Pure functions --
filterDirs :: [String] -> [FilePath] -> [FilePath]
filterDirs filters dirs = foldr (\f -> filter $ isInfixOf f) dirs filters

pruneImplicitDirs :: [FilePath] -> [FilePath]
pruneImplicitDirs = filter (\p -> p /= "." && p /= "..")

feedFilePath :: FeedDir -> FilePath
feedFilePath dir = joinPath [dir, feedFileName]

-- IO functions --
getFeedDirs :: IO [FeedDir]
getFeedDirs = do
  dirs <- fmap pruneImplicitDirs $ getDirectoryContents "."
  filterM doesDirectoryExist dirs

getFeedUrls :: [FeedDir] -> IO [Maybe String]
getFeedUrls = MP.mapM getUrl
  where getUrl dir = catch (getUrlThrow dir) errHandler
        getUrlThrow dir = do
          let content = readFile $ feedFilePath dir
          fmap (Just . head . lines) content
        errHandler :: IOError -> IO (Maybe a)
        errHandler _ = return Nothing

validFeedDir :: (FeedDir,Maybe String) -> Bool
validFeedDir (_,Nothing) = False
validFeedDir (_,Just _) = True


-- Main --
main = do
  putStrLn "Hello, world"
  args <- getArgs
  feedDirs <- fmap (filterDirs args) getFeedDirs
  feedUrls <- getFeedUrls feedDirs
  let feedDirsWithUrls = map extractUrl $ filter validFeedDir $ zip feedDirs feedUrls
  mapM_ (putStrLn . show) feedDirsWithUrls
      where extractUrl (dir,url) = (dir,fromJust url)

--getDirs :: IO [FilePath]
--getDirs = do
--  dirs <- fmap pruneImplicitDirs $ getDirectoryContents "."
--  filterM doesDirectoryExist dirs

--readFeedAddress :: FilePath -> IO String
--readFeedAddress = fmap head . fmap lines . readFile

--feedPathToAddress :: (FilePath,FilePath) -> IO (FilePath, String)
--feedPathToAddress (a,b) = do
--  feedAddress <- readFeedAddress b
--  return (a,feedAddress)

--getFeed :: FeedDir -> IO (Either String Feed)
--getFeed FeedDir { dir = dir,  feedUrl = url } = do
--  res <- openAsFeed url
--  putStrLn $ "Checked " ++ dir
--  return res

--pruneExistungUrls :: (FeedDir,[String],[FilePath]) -> (FeedDir,[String])
--pruneExistungUrls (fd,urls,dirEnts) = (fd,pruneHelper dirEnts urls [])
--  where pruneHelper _ [] acc = acc
--        pruneHelper dirEnts (url:xs) acc =
--          let toCheck = takeFileName url in
--            if any (\x -> x == toCheck || replace ".torrent" "" x == toCheck) dirEnts
--              then acc
--              else pruneHelper dirEnts xs (url:acc)

--getDirEnts :: FeedDir -> IO [FilePath]
--getDirEnts FeedDir { dir = dir, feedUrl = _ } = getDirectoryContents dir

--printFeedData :: [(FeedDir,[String])] -> IO ()
--printFeedData xs = mapM_ h xs
--  where h (fd,[])   = return ()
--        h (fd,urls) = do
--                        putStrLn $ "> Updates for " ++ (dir fd)
--                        mapM_ (putStrLn . takeFileName) urls

--downloadFeedData :: [(FeedDir,[String])] -> IO ()
--downloadFeedData xs = mapM_ h xs
--  where h (fd,urls) = mapM_ (\url -> download (dir fd) url) urls

--download :: FilePath -> String -> IO ()
--download destDir src = do
--  createProcess $ proc "aria2c" ["--file-allocation=none", "--seed-time=0", src, "-d", destDir]
--  return ()

--main = do
--  args <- getArgs
--  dirs <- liftM (filterDirs args) getDirs
--  feedPaths <- filterM (\(_,p) -> doesFileExist p) $ zip dirs $ map feedFilePath dirs
--  feedUrls <- fmap (map mkFeedDir) $ mapM feedPathToAddress feedPaths
--  dirEnts <- newEmptyMVar

--  forkIO $ do
--    tmp <- mapM getDirEnts feedUrls
--    putMVar dirEnts $! tmp

--  feeds <- MP.mapM getFeed feedUrls
--  putStrLn "All feeds down"

--  dents <- takeMVar dirEnts

--  let feedData = zip3 feedUrls (map getEnclosures feeds) dents
--  let feedDataPruned = parMap rdeepseq pruneExistungUrls feedData

--  if isFeedDataEmpty feedDataPruned
--  then return ()
--  else do
--    printFeedData feedDataPruned
--    putStr "Download? (Y/n) > "
--    hFlush stdout
--    answer <- getLine
--    if answer /= "" && (head answer == 'n' || head answer == 'N')
--    then putStrLn "Dann nicht..."
--    else downloadFeedData feedDataPruned


--getEnclosures :: Either String Feed -> [String]
--getEnclosures (Left _) = []
--getEnclosures (Right feed) = catMaybes $ map itemToEnclosure $ feedItems feed

--isFeedDataEmpty :: [(FeedDir,[String])] -> Bool
--isFeedDataEmpty xs = all ((==) []) $ map h xs
--  where h (fd,urls) = urls

--itemToEnclosure :: Item -> Maybe String
--itemToEnclosure (XMLItem element) =
--  let enclosureElement = findElement (QName "enclosure" Nothing Nothing) element in
--    Control.Monad.join $ fmap (findAttr (QName "url" Nothing Nothing)) enclosureElement
--itemToEnclosure item =
--    case (getItemEnclosure item) of
--      Nothing -> Nothing
--      Just (url,_,_) -> Just url

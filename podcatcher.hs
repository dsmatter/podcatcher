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

--
-- Globals --
--
feedFileName = "feed.url"

--
-- Types --
--
type FeedDir = FilePath
type FeedUrl = String
type FeedFile = FilePath

--
-- Data --
--
data PodcastFeed = PodcastFeed { dir :: FeedDir, url :: FeedUrl, localFiles :: [FeedFile], remoteFiles :: [FeedFile] }
  deriving (Show,Eq)

-- Some helper constructors
newPodcastFeed :: FeedDir -> PodcastFeed
newPodcastFeed dir = PodcastFeed dir "" [] []

mkPodcastFeed :: (FeedDir,FeedUrl) -> [FeedFile] -> PodcastFeed
mkPodcastFeed (dir,url) localFiles = PodcastFeed dir url localFiles []

addRemoteFiles :: PodcastFeed -> [FeedFile] -> PodcastFeed
addRemoteFiles pf = PodcastFeed (dir pf) (url pf) (localFiles pf)


--
-- Pure functions --
--

-- Directory handling
filterDirs :: [String] -> [FilePath] -> [FilePath]
filterDirs filters dirs = foldr (\f -> filter $ isInfixOf f) dirs filters

pruneImplicitDirs :: [FilePath] -> [FilePath]
pruneImplicitDirs = filter (\p -> p /= "." && p /= "..")

feedFilePath :: FeedDir -> FilePath
feedFilePath dir = joinPath [dir, feedFileName]

-- Pruning of Nothing entries
validFeedDir :: (FeedDir,Maybe String) -> Bool
validFeedDir (_,Nothing) = False
validFeedDir (_,Just _) = True

validFeedDirs :: [(FeedDir,Maybe String)] -> [(FeedDir,String)]
validFeedDirs = map extractUrl . filter validFeedDir
  where extractUrl (dir,url) = (dir,fromJust url)

validRss :: [(PodcastFeed,Maybe Feed)] -> [(PodcastFeed,Feed)]
validRss = map extractFeed . filter isRssValid
  where isRssValid (_,Nothing) = False
        isRssValid (_,Just _) = True
        extractFeed (p,f) = (p,fromJust f)

-- Extract the URLs from RSS
itemToEnclosure :: Item -> Maybe String
itemToEnclosure (XMLItem element) =
  let enclosureElement = findElement (QName "enclosure" Nothing Nothing) element in
    Control.Monad.join $ fmap (findAttr (QName "url" Nothing Nothing)) enclosureElement
itemToEnclosure item =
    case (getItemEnclosure item) of
      Nothing -> Nothing
      Just (url,_,_) -> Just url

-- Add remote files to a PodcastFeed
getRemoteFiles :: [(PodcastFeed,Feed)] -> [PodcastFeed]
getRemoteFiles = map remoteFiles
  where remoteFiles (pf,f) = let rf = catMaybes $ map itemToEnclosure $ feedItems f
                             in addRemoteFiles pf rf

-- Append ".torrent"
mkTorrent :: FeedFile -> FeedFile
mkTorrent = (++".torrent")

-- Compare the file name of two file paths while being isomorph on .torrent extension
eqFeedFile :: FeedFile -> FeedFile -> Bool
eqFeedFile a b = let aa = takeFileName a
                 in let bb = takeFileName b
                    in aa == bb || (mkTorrent aa) == bb || aa == (mkTorrent bb)

-- Append a list of new files to a PodcastFeed
getNewFiles :: [PodcastFeed] -> [(PodcastFeed,[String])]
getNewFiles = map result
  where result pf = (pf,newFiles (remoteFiles pf) (localFiles pf))
        newFiles = h []
        h acc [] _ = acc
        h acc (rf:rfs) lf = if any (eqFeedFile rf) lf
                             then acc
                             else h (rf:acc) rfs lf

--
-- IO functions --
--
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

getFeedLocalFiles :: [FeedDir] -> IO [[FeedFile]]
getFeedLocalFiles = MP.mapM getFeedContent
  where getFeedContent = fmap reverse . getDirectoryContents

getOneFeedRss :: PodcastFeed -> IO (Maybe Feed)
getOneFeedRss feed = do
  rss <- openAsFeed $ url feed
  case rss of
    Left e -> putStrLn e >> return Nothing
    Right f -> putStrLn ("Checked " ++ dir feed) >> return (Just f)

getFeedRss :: [PodcastFeed] -> IO [Maybe Feed]
getFeedRss = MP.mapM getOneFeedRss

getPodcastFeeds :: [FeedDir] -> IO [PodcastFeed]
getPodcastFeeds dirs = do
  -- Try to get all feed URLs
  feedUrls <- getFeedUrls dirs

  -- Link directory to URL and prune invalid feeds (i.e. the ones without a feed URL)
  let feedDirsWithUrls = validFeedDirs $ zip dirs feedUrls

  -- Get the local feed files for each directory
  feedLocalFiles <- getFeedLocalFiles $ map fst feedDirsWithUrls

  -- Make a podcast feed (w/o any remote files, yet)
  let feeds = zipWith mkPodcastFeed feedDirsWithUrls feedLocalFiles

  -- Get RSS for each feed
  feedRss <- getFeedRss feeds

  -- Link feeds to RSS and prune invalid entries
  let feedsWithRss = validRss $ zip feeds feedRss

  -- Make podcast feeds with all information
  return $ getRemoteFiles feedsWithRss

showNewFiles :: (PodcastFeed,[String]) -> IO ()
showNewFiles (_,[]) = return ()
showNewFiles (pf,nf) = do
  putStrLn $ "> " ++ (dir pf) ++ ":"
  mapM_ (putStrLn . takeFileName) nf

download :: FilePath -> String -> IO ()
download destDir src = do
  createProcess $ proc "aria2c" ["--file-allocation=none", "--seed-time=0", "-d", destDir, "-o", (takeFileName src), src]
  return ()

downloadAll :: [(PodcastFeed,[String])] -> IO ()
downloadAll = mapM_ dl
  where dl (pf,dfs) = mapM_ (\df -> download (dir pf) df) dfs

--
-- Main --
--
main = do
  args <- getArgs
  dirs <- fmap (filterDirs args) getFeedDirs
  feeds <- getPodcastFeeds dirs
  let result = getNewFiles feeds

  -- Show files and ask
  putStrLn "=== New episodes ==="
  mapM_ showNewFiles result
  putStr "Download? (Y/n) > "
  hFlush stdout
  answer <- getLine

  -- Act accordingly
  if answer /= "" && (head answer == 'n' || head answer == 'N')
  then putStrLn "Dann nicht..."
  else downloadAll result

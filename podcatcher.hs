import Prelude hiding (catch)
import Control.Applicative
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
import qualified Control.Monad.Parallel as MP

feedFileName = "feed.url"

type FeedDir = FilePath
type FeedUrl = String
type FeedFile = FilePath

data Podcast = Podcast { podcastDir :: FeedDir, podcastFeedUrl :: FeedUrl }
  deriving (Show, Eq)

-- FIXME: episode name
data Episode = Episode { episodeUrl :: FeedUrl, episodePath :: FilePath }
  deriving (Show, Eq)

main = do
  args <- getArgs
  podcasts <- getPodcasts args
  newEpisodes <- concat <$> MP.mapM getNewEpisodes podcasts

  case newEpisodes of
    [] -> putStrLn "Maybe next time :/"
    _ -> do
      putStrLn "=== New episodes ==="
      showEpisodes newEpisodes
      putStr "Download? (Y/n) > "
      hFlush stdout
      answer <- getLine
      -- Act accordingly
      if answer /= "" && (head answer == 'n' || head answer == 'N')
        then putStrLn "kthxbye"
        else downloadAll newEpisodes

--
-- Pure functions --
--

-- Directory handling
filterDirs :: [String] -> [FilePath] -> [FilePath]
filterDirs [] = id
filterDirs filters = filter matchesAnyFilter
  where matchesAnyFilter dir = any (flip isInfixOf dir) filters

pruneImplicitDirs :: [FilePath] -> [FilePath]
pruneImplicitDirs = filter (\p -> p /= "." && p /= "..")

feedFilePath :: FeedDir -> FilePath
feedFilePath dir = joinPath [dir, feedFileName]

---- Extract the URLs from RSS
itemToEnclosure :: Item -> Maybe String
itemToEnclosure (XMLItem element) =
  findElement (QName "enclosure" Nothing Nothing) element >>= findAttr (QName "url" Nothing Nothing)
itemToEnclosure item =
    case (getItemEnclosure item) of
      Nothing -> Nothing
      Just (url,_,_) -> Just url

----
---- IO functions --
----
getLocalDirs :: IO [FeedDir]
getLocalDirs = do
  dirs <- pruneImplicitDirs <$> getDirectoryContents "."
  filterM doesDirectoryExist dirs

getFeedUrl :: FeedDir -> IO (Maybe String)
getFeedUrl dir = catch readFeedUrlFile errHandler
  where readFeedUrlFile = Just . head . lines <$> readFile (feedFilePath dir)
        errHandler :: IOError -> IO (Maybe String)
        errHandler _ = return Nothing

getPodcasts :: [String] -> IO [Podcast]
getPodcasts filters = do
  ldirs <- filterDirs filters <$> getLocalDirs
  mpodcasts <- MP.mapM toPodcast ldirs
  return $ catMaybes mpodcasts
    where toPodcast dir = do
            feedUrl <- getFeedUrl dir
            return $ (Podcast dir) <$> feedUrl

getNewEpisodes :: Podcast -> IO [Episode]
getNewEpisodes (Podcast pdir purl) = do
  mrss <- openAsFeed purl
  putStrLn $ "Checking " ++ pdir
  case mrss of
    Left _ -> return []
    Right rss -> do
      let episodeUrls = catMaybes $ map itemToEnclosure $ feedItems rss
      let episodes = map toEpisode episodeUrls
      newEpisodes <- takeWhileM (fmap not . episodeExists) episodes
      return newEpisodes
        where toEpisode eurl = Episode eurl $ joinPath [pdir, takeFileName eurl]

episodeExists :: Episode -> IO Bool
episodeExists (Episode _ fn) =
  let fnt = replace ".torrent" "" fn
  in or <$> sequence [doesFileExist fn, doesFileExist fnt]

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (x:xs) = do
  flg <- p x
  if flg
    then liftM (x:) $ takeWhileM p xs
    else return []

showEpisodes :: [Episode] -> IO ()
showEpisodes = mapM_ (putStrLn . episodePath)

downloadEpisode :: Episode -> IO ProcessHandle
downloadEpisode (Episode url dst) = do
  (_,_,_,h) <- createProcess $ proc "aria2c" ["--file-allocation=none", "--seed-time=0", "-d", (takeDirectory dst), "-o", (takeFileName dst), url]
  return h

downloadAll :: [Episode] -> IO ()
downloadAll es = do
  phs <- mapM downloadEpisode es
  mapM_ waitForProcess phs


import Control.Monad
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

feedFileName = "feed.url"

data FeedDir = FeedDir { dir :: FilePath, feedUrl :: String } deriving (Show, Eq)

instance NFData FeedDir where
  rnf (FeedDir dir url) = rnf dir `seq` rnf url `seq` ()

getDirs :: IO [FilePath]
getDirs = do
  dirs <- fmap pruneImplicitDirs $ getDirectoryContents "."
  filterM doesDirectoryExist dirs

readFeedAddress :: FilePath -> IO String
readFeedAddress = fmap head . fmap lines . readFile

feedPathToAddress :: (FilePath,FilePath) -> IO (FilePath, String)
feedPathToAddress (a,b) = do
  feedAddress <- readFeedAddress b
  return (a,feedAddress)

getFeed :: FeedDir -> IO (Either String Feed)
getFeed FeedDir { dir = dir,  feedUrl = url } = do
  res <- openAsFeed url
  putStrLn $ "Checked " ++ dir
  return res

pruneExistungUrls :: (FeedDir,[String],[FilePath]) -> (FeedDir,[String])
pruneExistungUrls (fd,urls,dirEnts) = (fd,pruneHelper dirEnts urls [])
  where pruneHelper _ [] acc = acc
        pruneHelper dirEnts (url:xs) acc =
          let toCheck = takeFileName url in
            if any (\x -> x == toCheck || replace ".torrent" "" x == toCheck) dirEnts
              then acc
              else pruneHelper dirEnts xs (url:acc)

getDirEnts :: FeedDir -> IO [FilePath]
getDirEnts FeedDir { dir = dir, feedUrl = _ } = getDirectoryContents dir

printFeedData :: [(FeedDir,[String])] -> IO ()
printFeedData xs = mapM_ h xs
  where h (fd,[])   = return ()
        h (fd,urls) = do
                        putStrLn $ "> Updates for " ++ (dir fd)
                        mapM_ (putStrLn . takeFileName) urls

downloadFeedData :: [(FeedDir,[String])] -> IO ()
downloadFeedData xs = mapM_ h xs
  where h (fd,urls) = mapM_ (\url -> download (dir fd) url) urls

download :: FilePath -> String -> IO ()
download destDir src = do
  createProcess $ proc "aria2c" ["--file-allocation=none", "--seed-time=0", src, "-d", destDir]
  return ()

main = do
  args <- getArgs
  dirs <- liftM (filterDirs args) getDirs
  feedPaths <- filterM (\(_,p) -> doesFileExist p) $ zip dirs $ map feedFilePath dirs
  feedUrls <- fmap (map mkFeedDir) $ mapM feedPathToAddress feedPaths
  dirEnts <- newEmptyMVar

  forkIO $ do
    tmp <- mapM getDirEnts feedUrls
    putMVar dirEnts $! tmp

  feeds <- MP.mapM getFeed feedUrls
  putStrLn "All feeds down"

  dents <- takeMVar dirEnts

  let feedData = zip3 feedUrls (map getEnclosures feeds) dents
  let feedDataPruned = parMap rdeepseq pruneExistungUrls feedData

  if isFeedDataEmpty feedDataPruned
  then return ()
  else do
    printFeedData feedDataPruned
    putStr "Download? (Y/n) > "
    hFlush stdout
    answer <- getLine
    if answer /= "" && (head answer == 'n' || head answer == 'N')
    then putStrLn "Dann nicht..."
    else downloadFeedData feedDataPruned

-- Pure functions
filterDirs :: [String] -> [FilePath] -> [FilePath]
filterDirs [] dirs = dirs
filterDirs (substr:xs) dirs = filter (isInfixOf substr) dirs

pruneImplicitDirs :: [FilePath] -> [FilePath]
pruneImplicitDirs = filter (\p -> p /= "." && p /= "..")

feedFilePath :: FilePath -> FilePath
feedFilePath dir = joinPath [dir, feedFileName]

mkFeedDir :: (FilePath,String) -> FeedDir
mkFeedDir (dir, feedUrl) = FeedDir dir feedUrl

getEnclosures :: Either String Feed -> [String]
getEnclosures (Left _) = []
getEnclosures (Right feed) = catMaybes $ map itemToEnclosure $ feedItems feed

isFeedDataEmpty :: [(FeedDir,[String])] -> Bool
isFeedDataEmpty xs = all ((==) []) $ map h xs
  where h (fd,urls) = urls

itemToEnclosure :: Item -> Maybe String
itemToEnclosure (XMLItem element) =
  let enclosureElement = findElement (QName "enclosure" Nothing Nothing) element in
    Control.Monad.join $ fmap (findAttr (QName "url" Nothing Nothing)) enclosureElement
itemToEnclosure item =
    case (getItemEnclosure item) of
      Nothing -> Nothing
      Just (url,_,_) -> Just url


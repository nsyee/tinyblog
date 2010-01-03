{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
module TinyBlog.Entry(
    Entry(..),
    getEntryFiles,
    readEntry,
    readEntries,
    entryToKV,
    sortByName,
    toJSON,
    toRSS
) where

import TinyBlog.Config
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.Object.Json as J
import System.Directory
import System.Posix.Files
import System.Time
import Data.Maybe
import Network.URI
import qualified System.IO.UTF8 as U
import qualified Text.RSS as R

data Entry = Entry
    { eCode  :: String        -- ^ entry code
    , eTitle :: String        -- ^ entry title
    , eBody  :: String        -- ^ entry body
    , eMtime :: CalendarTime  -- ^ entry mtime
    , eUrl   :: String        -- ^ entry url
    } deriving (Eq, Show)


-- | get entry file list
getEntryFiles :: IO [FilePath]
getEntryFiles = do
    curDir <- getCurrentDirectory
    cs <- getDirectoryContents $ curDir ++ "/data"
    let fs = map ("data/" ++) $ removeSwapFile $ removeDotFile cs
    stats <- mapM getFileStatus fs
    let results = sortByName $ zip stats fs
    return $ map (\(_, f) -> f) results
    where
        removeSwapFile = filter ((/=) '.' . head)
        removeDotFile = filter (`notElem` [".", ".."])

-- | read and make entry object
readEntry :: FilePath -> IO (Maybe Entry)
readEntry file = do
    exist <- doesFileExist file
    if exist
        then do
            ct <- U.readFile file
            stat  <- getFileStatus file
            let cs = lines ct
                c = getCode file
                t = head cs
                b = (unlines . tail . tail) cs
                m = getMtime stat
                u = "/" ++ c ++ "/"
            return $ Just $ Entry c t b m u
        else return $ Nothing
            where
                getCode = takeWhile (/= '.') . tail . dropWhile (/= '/')
                getMtime s =
                    toUTCTime $ TOD (truncate $ toRational $ modificationTime s) 0


readEntries :: [FilePath] -> IO [Entry]
readEntries files = do
            mEntries <- mapM readEntry files
            return $ map fromJust $ filter isJust mEntries


entryToKV :: Entry -> [(String, String)]
entryToKV (Entry c t b m u) =
    [ ("code" ,c)
    , ("title",t)
    , ("body" ,b)
    , ("mtime",calendarTimeToString m)
    , ("url"  ,u)
    ]

-- | make entry list
listKV :: [Entry] -> [[(String, String)]]
listKV entries = map mkEntry entries
            where mkEntry (Entry c t b m u) =
                    [ ("code", c)
                    , ("title"  , t)
                    , ("mtime" ,calendarTimeToString m)
                    , ("url", u)
                    ]

-- | sort entries by name
sortByName :: [(FileStatus, FilePath)]
           -> [(FileStatus, FilePath)]
sortByName = reverse

-- | sort entries by mtime
{--
sortByMtime :: [(FileStatus, FilePath)]
            -> [(FileStatus, FilePath)]
sortByMtime [] = []
sortByMtime (x@(xstat, _):xs) = greater ++ x:lesser
    where lesser =
            sortByMtime [y | y@(ystat, _) <- xs,
                             (modificationTime ystat) <  (modificationTime xstat)
                        ]
          greater =
            sortByMtime [y | y@(ystat, _) <- xs,
                             (modificationTime ystat) >= (modificationTime xstat)
                        ]
--}

-- | get a JSON of entries
toJSON :: [Entry] -> B.ByteString
toJSON entries = J.encode $ J.toJsonObject $ map J.toJsonObject $ listKV entries

-- | get a RSS of entries
toRSS :: [Entry] -> Config -> B.ByteString
toRSS entries cfg = (B.fromString . R.showXML . R.rssToXML) rss
    where
        rss = R.RSS title link description channel items
        title = cBlogName cfg
        link = URI
            { uriScheme = "http://"
            , uriPath = cHostName cfg
            , uriAuthority = Nothing
            , uriQuery = ""
            , uriFragment = ""
            }
        description = cDescription cfg
        channel =
            [ R.Copyright $ cCopyright cfg
            , R.WebMaster $ cEmail cfg
            , R.LastBuildDate $ eMtime $ head entries
            ]
        items = map mkItem $ take 20 entries
        mkItem e =
            [ R.Title $ eTitle e
            , R.Description $ eBody e
            , R.Author $ cAuthorName cfg
            , R.Link entryLink
            , R.Guid True (show entryLink)
            , R.PubDate $ eMtime e
            ]
            where entryLink = URI
                    { uriScheme = "http://"
                    , uriPath = (cHostName cfg) ++ "/" ++ (eCode e)
                    , uriAuthority = Nothing
                    , uriQuery = ""
                    , uriFragment = ""
                    }

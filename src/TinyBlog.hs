module TinyBlog(
    module TinyBlog.Config,
    TinyBlog.app
) where

import TinyBlog.Config
import TinyBlog.Entry
import Data.Maybe
import Network.Gravatar
import Network.Loli
import Network.Loli.Template.TextTemplate
import Network.Loli.Utils
--import Hack.Contrib.Request
import Hack.Contrib.Response
import Hack
--import Control.Monad.Reader

-- | *application main
app :: Config -> Hack.Env -> IO Hack.Response
app cfg = loli $ do

    layout "layout.html"

    -- | JSON response
    get "/json/entries" $ do
        -- get count from querystring
        --env <- ask
        --let count = getCount env

        -- get recent entries
        files <- io $ getEntryFiles
        entries <- io $ readEntries files

        no_layout $ do
            update $ set_content_type "application/json; charset=utf-8"
            update $ set_body $ toJSON entries

    -- | RSS response
    get "/rss" $ do
        -- get recent entries
        files <- io $ getEntryFiles
        entries <- io $ readEntries files

        no_layout $ do
            update $ set_content_type "application/xml; charset=utf-8"
            update $ set_body $ toRSS entries cfg

    -- | HTML response
    get "/:entry" $ do
        ps <- captures
        let entryCode = fromJust $ lookup "entry" ps
        maybeEntry <- io $ readEntry $ "data/" ++ entryCode ++ ".txt"

        update $ set_content_type "text/html"

        case maybeEntry of
            Just entry -> do
                let gravatarImage = getGravatarImage (cUseGravatar cfg) (cEmail cfg)
                context ((entryToKV entry) ++
                    [ ("metaTitle", eTitle entry ++ " - " ++ (cBlogName cfg))
                    , ("gravatar", gravatarImage)
                    ]) $ do
                    output  $ text_template "entry.html"
            Nothing -> do
                update $ set_status 404
                no_layout $ output $ text_template "404.html"

    get "/" $ do
        -- get a latest entry
        files <- io $ getEntryFiles
        entries <- io $ readEntries files
        let entry = head entries
        let gravatarImage = getGravatarImage (cUseGravatar cfg) (cEmail cfg)

        update $ set_content_type "text/html"
        context ((entryToKV entry) ++
            [ ("metaTitle", cBlogName cfg)
            , ("gravatar", gravatarImage)
            ]) $ do
            output  $ text_template "index.html"

    public (Just ".") ["/static"]



-- | get a gravatar image
getGravatarImage :: Bool -> String -> String
getGravatarImage True email = gravatar email
getGravatarImage False _    = ""


-- | get count from querystring
{-
getCount :: Hack.Env -> Int
getCount env =
    let ps = params env
    in
        case lookup "count" ps of
            Just count -> read count
            _          -> 20
-}

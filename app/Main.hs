module Main where
import TinyBlog
import Hack.Handler.SimpleServer

main :: IO ()
main = do
    let cfg = Config
            { cBlogName    = "Sample Blog"
            , cHostName    = "sample.com"
            , cAuthorName  = "your name"
            , cCopyright   = "copyright 2010 foo name all rights reserved."
            , cEmail       = "foo@sample.com"
            , cDescription = "On Loving Lambda"
            , cUseGravatar = True
            }
    let port = 3000  -- port number
    run port $ TinyBlog.app cfg

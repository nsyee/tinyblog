module Main where
import TinyBlog
import Hack.Handler.Hyena

main :: IO ()
main = do
    let cfg = Config
            { cBlogName = "Sample Blog"
            , cHostName = "sample.com"
            , cAuthorName = "foo name"
            , cCopyright = "copyright 2010 foo name all rights reserved."
            , cEmail = "foo@sample.com"
            , cDescription = "On Loving Lambda"
            }
    run $ TinyBlog.app cfg

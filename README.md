# tinyblog

A simple blog tool written with Haskell.

## Install and compile:

    %git clone git://github.com/nsyee/tinyblog.git
    %cd tinyblog
    %runghc Setup.hs configure
    %runghc Setup.hs build
    %runghc Setup.hs install

## Make your app like this:

    -- myapp.hs
    module Main where
    import TinyBlog
    import Hack.Handler.SimpleServer -- use any Hack.Handler you like.

    main :: IO ()
    main = do
        let cfg = Config
                { cBlogName    = "Sample Blog" -- ^ blog name
                , cHostName    = "sample.com"-- ^ host name
                , cAuthorName  = "your name"-- ^ author name
                , cCopyright   = "copyright 2010 foo name all rights reserved." -- ^ copyright
                , cEmail       = "foo@sample.com" -- ^ email
                , cDescription = "On Loving Lambda" -- ^ description of this blog
                , cUseGravatar = True -- ^ use gravatar image or not (http://gravatar.com)
                }
        let port = 3000  -- port number
        run port $ TinyBlog.app cfg
    -- /myapp.hs

    %ghc --make myapp.hs
    %./myapp

## Make html templates. See app/views dir. They are samples.

check: <http://localhost:3000>


## See also
* tinyblog is using [loli](http://github.com/nfjinjing/loli) (Web Application Framework).

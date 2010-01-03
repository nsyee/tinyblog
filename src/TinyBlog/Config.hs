module TinyBlog.Config(
    Config(..)
) where

data Config = Config
    { cBlogName    :: String  -- ^ blog name
    , cHostName    :: String  -- ^ host name
    , cAuthorName  :: String  -- ^ author name
    , cCopyright   :: String  -- ^ copyright
    , cEmail       :: String  -- ^ email
    , cDescription :: String  -- ^ description of this blog
    } deriving (Eq, Show)

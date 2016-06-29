module Blog.Types where

type PostId = Int
type Date = (Int, Int, Int) -- Year, Month, Day
type PostContent = String

data PostInfo = PostInfo
  { postId :: PostId
  , postDate :: Date
  , postTopic :: String
  } deriving (Eq, Show)

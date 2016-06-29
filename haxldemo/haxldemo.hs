{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
    OverloadedStrings, RecordWildCards #-}

import Haxl.Core hiding (Request)
import Data.Typeable
import Data.Hashable
import Data.Monoid
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Control.Concurrent.Async

type PostId = Int
type Date = (Int, Int, Int) -- Year, Month, Day
type PostContent = String
type Html = String

data PostInfo = PostInfo
  { postId :: PostId
  , postDate :: Date
  , postTopic :: String
  } deriving (Eq, Show)

data Request a where
  FetchPosts :: Request [PostId]
  FetchPostInfo :: PostId -> Request PostInfo
  FetchPostContent :: PostId -> Request PostContent
  FetchPostViews :: PostId -> Request Int
  deriving Typeable

deriving instance Eq (Request a)
deriving instance Show (Request a)
instance Show1 Request where show1 = show

instance Hashable (Request a) where
  hashWithSalt s FetchPosts      = hashWithSalt s (0::Int)
  hashWithSalt s (FetchPostInfo id) = hashWithSalt s (1::Int, id)
  hashWithSalt s (FetchPostContent id) = hashWithSalt s (2::Int, id)
  hashWithSalt s (FetchPostViews id) = hashWithSalt s (3::Int, id)

instance DataSourceName Request where
  dataSourceName _ = "Blog"

instance DataSource u Request where
  fetch _state _flags _user bfs = AsyncFetch $ \inner -> do
    putStrLn "fetch>>>"
    putStrLn $ unwords [show req | (BlockedFetch req ref) <- bfs]
    inner
    mapM_ (\r -> putSuccess r [1..10]) allIds
    mapM_ (uncurry putSuccess) (zip infoVars (map postInfo infoIds))
    mapM_ (uncurry putSuccess) (zip contentVars (map postContent contentIds))
    mapM_ (uncurry putSuccess) (zip viewsVars (map postViews viewsIds))
    putStrLn "fetch<<<"
    return ()
    where
      allIds :: [ResultVar [PostId]]
      allIds = [r | BlockedFetch FetchPosts r <- bfs]
      infoIds :: [PostId]
      infoVars :: [ResultVar PostInfo]
      (infoIds, infoVars) = unzip [(id,r) | BlockedFetch (FetchPostInfo id) r <- bfs]
      contentIds :: [PostId]
      contentVars :: [ResultVar PostContent]
      (contentIds, contentVars) = unzip [(id,r) | BlockedFetch (FetchPostContent id) r <- bfs]
      viewsIds :: [PostId]
      viewsVars :: [ResultVar Int]
      (viewsIds, viewsVars) = unzip [(id,r) | BlockedFetch (FetchPostViews id) r <- bfs]

instance StateKey Request where
  data State Request = BlogState {}

getPostIds = dataFetch FetchPosts
getPostInfo = dataFetch . FetchPostInfo
getPostContent = dataFetch . FetchPostContent
getPostViews = dataFetch . FetchPostViews

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts posts = foldMap render posts
  where render (PostInfo{..}, content) = "<post>" <> show postDate <> "|" <> show postTopic <> "|" <> content <> "</post>"

renderPage :: Html -> Html -> Html
renderPage left main = "<page><left>" <> left <> "</left><main>" <> main <> "</main></page>"

blog :: GenHaxl u Html
blog = renderPage <$> leftPane <*> mainPane

leftPane :: GenHaxl u Html
leftPane = renderSidePane <$> popularPosts <*> topics

mainPane :: GenHaxl u Html
mainPane = do
  posts <- getAllPostsInfo
  let ordered = take 5 $ sortBy (flip (comparing postDate)) posts
  content <- mapM (getPostContent . postId) ordered
  return $ renderPosts (zip ordered content)

getAllPostsInfo :: GenHaxl u [PostInfo]
getAllPostsInfo = mapM getPostInfo =<< getPostIds

popularPosts :: GenHaxl u Html
popularPosts = do
  pids <- getPostIds
  views <- mapM getPostViews pids
  let ordered = take 5 $ map fst $ sortBy (flip (comparing snd)) (zip pids views)
  content <- mapM getPostDetails ordered
  return $ renderPosts content

getPostDetails :: PostId -> GenHaxl u (PostInfo, PostContent)
getPostDetails pid = (,) <$> getPostInfo pid <*> getPostContent pid

topics :: GenHaxl u Html
topics = do
  posts <- getAllPostsInfo
  let topiccounts = Map.fromListWith (+) [(postTopic p, 1) | p <- posts]
  return $ renderTopics topiccounts

renderTopics :: Map.Map String Int -> Html
renderTopics topiccounts = Map.showTree topiccounts

renderSidePane :: Html -> Html -> Html
renderSidePane pop topic = "<side><popular>" <> pop <> "</popular><topics>" <> topic <> "</topics></side>"

postInfo :: PostId -> PostInfo
postInfo id = PostInfo {
  postId = id
  , postDate = (2016, 6, id)
  , postTopic = show id
}

postContent :: PostId -> PostContent
postContent = ("content" ++). show

postViews :: PostId -> Int
postViews = id


main :: IO ()
main = do
  let stateStore = stateSet BlogState {} stateEmpty
  env0 <- initEnv stateStore ()
  html <- runHaxl env0 blog
  print html
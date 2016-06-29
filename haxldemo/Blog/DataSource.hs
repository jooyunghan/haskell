{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
    OverloadedStrings, RecordWildCards #-}

module Blog.DataSource
  ( Request(..)
  , initialState
  ) where

import Haxl.Core hiding (Request)
import Data.Typeable
import Data.Hashable
import Data.Monoid
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Control.Concurrent.Async
import Blog.Types

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
    mapM_ (\r -> putSuccess r postIds) allIds
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

initialState :: IO (State Request)
initialState = return (BlogState {})

-- mock data --

postIds :: [PostId]
postIds = [1..10]

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

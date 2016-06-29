{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
    OverloadedStrings, RecordWildCards, ApplicativeDo #-}

import Prelude hiding (mapM)
import Haxl.Core
import Haxl.Prelude
import Data.Monoid
import Data.List
import Data.Ord
import qualified Data.Map as Map

import Blog.DataSource
import Blog

type Html = String

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

-- render --

renderTopics :: Map.Map String Int -> Html
renderTopics topiccounts = Map.showTree topiccounts

renderSidePane :: Html -> Html -> Html
renderSidePane pop topic = "<side><popular>" <> pop <> "</popular><topics>" <> topic <> "</topics></side>"

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts posts = foldMap render posts
  where render (PostInfo{..}, content) = "<post>" <> show postDate <> "|" <> show postTopic <> "|" <> content <> "</post>"

renderPage :: Html -> Html -> Html
renderPage left main = "<page><left>" <> left <> "</left><main>" <> main <> "</main></page>"

-- main --

main :: IO ()
main = do
  state <- initialState
  let stateStore = stateSet state stateEmpty
  env0 <- initEnv stateStore ()
  html <- runHaxl env0 blog
  putStrLn html
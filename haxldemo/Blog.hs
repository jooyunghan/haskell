module Blog
  ( getPostIds
  , getPostInfo
  , getPostContent
  , getPostViews
  , PostId(..), PostInfo(..), PostContent(..)
  ) where

import Blog.DataSource
import Blog.Types (PostId(..), PostInfo(..), PostContent(..))
import Haxl.Core

-- DSL --

getPostIds = dataFetch FetchPosts
getPostInfo = dataFetch . FetchPostInfo
getPostContent = dataFetch . FetchPostContent
getPostViews = dataFetch . FetchPostViews

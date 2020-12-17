{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api
  ( api
  , Episode (..)
  ) where

import           Data.Proxy
import           Data.Text               (Text)
import qualified Data.ByteString.Lazy          as Lazy
import           Network.HTTP.Media      ((//), (/:))
import           Servant.API
import           Servant.Multipart
import           Servant.Server
import           Text.Blaze.Html         (Html)

import           Prelude                 (FilePath, String, id, (.))

data Episode = Episode
    { epTitle :: Text
    , epFile  :: FilePath
    }

type API = "feed.xml" :> Get '[XML] Lazy.ByteString
      :<|> "upload"   :> Get '[HTML] Lazy.ByteString
      :<|> "upload"   :> MultipartForm Tmp (MultipartData Tmp) :> Post '[PlainText] String

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Lazy.ByteString where
  mimeRender _ = id

data XML

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance MimeRender XML Lazy.ByteString where
  mimeRender _ = id

api :: Proxy API
api = Proxy

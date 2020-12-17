{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api
  ( api
  , EpisodeUpload (..)
  ) where

import           Data.Proxy
import           Data.Text               (Text)
import qualified Data.ByteString.Lazy          as Lazy
import           Network.HTTP.Media      ((//), (/:))
import           Servant.API
import           Servant.Multipart
import           Servant.Server
import           Text.Blaze.Html         (Html)

import           Prelude                 (FilePath, String, id, (.), (<$>), (<*>), fmap)

data EpisodeUpload = EpisodeUpload
    { epTitle :: Text
    , epAudioFile  :: FilePath
    , epAudioFilename :: Text
    , epDescription :: Text
    , epThumbnailFile :: FilePath
    , epThumbnailFilename :: Text
    , epAudioContentType :: Text
    }

instance FromMultipart Tmp EpisodeUpload where
  fromMultipart formdata =
    let audioFile = lookupFile "audioFile" formdata
        thumbnailFile = lookupFile "thumbnailFile" formdata
    in  EpisodeUpload <$> lookupInput "title" formdata
                      <*> fmap fdPayload audioFile
                      <*> fmap fdFileName audioFile
                      <*> lookupInput "description" formdata
                      <*> fmap fdPayload thumbnailFile
                      <*> fmap fdFileName thumbnailFile
                      <*> fmap fdFileCType audioFile

type API = "feed.xml" :> Get '[XML] Lazy.ByteString
      :<|> "upload"   :> Get '[HTML] Lazy.ByteString
      :<|> "upload"   :> MultipartForm Tmp EpisodeUpload :> Post '[PlainText] String

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

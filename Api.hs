{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api
  ( api
  , EpisodeUpload (..)
  ) where

import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy
import           Data.Text            (Text)
import           Data.Text.Read       (decimal)
import           Network.HTTP.Media   ((//), (/:))
import           Servant.API
import           Servant.Multipart
import           Servant.Server
import           Text.Blaze.Html      (Html)

import           Prelude              (Either (..), FilePath, Int, Maybe (..),
                                       String, fmap, id, (.), (<$>), (<*>))

data EpisodeUpload = EpisodeUpload
    { uploadTitle             :: Text
    , uploadDate              :: Text
    , uploadAudioFile         :: FilePath
    , uploadAudioFilename     :: Text
    -- audio duration in seconds
    , uploadDuration          :: Int -- audio duration in seconds
    , uploadDescription       :: Text
    , uploadThumbnailFile     :: FilePath
    , uploadThumbnailFilename :: Text
    , uploadAudioContentType  :: Text
    }

instance FromMultipart Tmp EpisodeUpload where
  fromMultipart formdata =
    let audioFile = lookupFile "audioFile" formdata
        thumbnailFile = lookupFile "thumbnailFile" formdata
        duration = case decimal <$> lookupInput "duration" formdata of
          Just (Right (d, _)) -> Just d
          _                   -> Nothing
    in  EpisodeUpload <$> lookupInput "title" formdata
                      <*> lookupInput "date" formdata
                      <*> fmap fdPayload audioFile
                      <*> fmap fdFileName audioFile
                      <*> duration
                      <*> lookupInput "description" formdata
                      <*> fmap fdPayload thumbnailFile
                      <*> fmap fdFileName thumbnailFile
                      <*> fmap fdFileCType audioFile

type API = "feed.xml" :> Get '[XML] Lazy.ByteString
      :<|> "upload"   :> Get '[HTML] Lazy.ByteString
      :<|> "upload"   :> MultipartForm Tmp EpisodeUpload :> Post '[PlainText] String
      :<|> Get '[HTML] Lazy.ByteString

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

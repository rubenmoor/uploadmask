{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Foldable                 (foldl')
import           Data.Text                     (Text, replace)
import qualified Data.Text                     as Text
import           Data.Text.IO                  (readFile)
import           Data.Time.Clock               (getCurrentTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime,
                                                rfc822DateFormat)
import           Data.Time.LocalTime           (getZonedTime)
import           Database.Persist              ()
import           Database.Persist.Sqlite       (runMigration, runSqlite)
import           Model                         (migrateAll)
import           Network.Wai.Handler.Warp      (defaultSettings, runSettings)
import           Servant                       ((:<|>) (..), Application,
                                                Handler)
import           Servant.Multipart             (MultipartData, Tmp)
import           Servant.Server                (serve)
import           System.Directory              (copyFile)
import           System.FilePath.Posix         (takeExtensions)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Renderer.Utf8      (renderMarkup)
import           Text.Heterocephalus

import           Api                           (EpisodeUpload (..), api)
import           Html                          (uploadForm)
import qualified Model

import           Prelude                       (FilePath, IO, String, return,
                                                ($), (<>))

convertToFilename :: Text -> Text
convertToFilename str =
  let map = [ ("Ä", "A")
            , ("Ö", "O")
            , ("Ü", "U")
            , ("ß", "SS")
            , ("?", "_")
            , ("!", "_")
            , (".", "_")
            , (";", "_")
            , (":", "_")
            , ("'", "_")
            , ("=", "_")
            , ("<", "_")
            , (">", "_")
            , ("/", "_")
            , ("\\", "_")
            , ("\"", "_")
            , ("&", "_")
            , ("@", "_")
            , ("%", "_")
            , ("+", "_")
            , ("*", "_")
            , ("$", "_")
            , (" ", "_")
            , ("(", "_")
            , (")", "_")
            ]
      acc str' (s, t) = replace s t str'
  in  foldl' acc str map

app :: Application
app = serve api $
       handleFeedXML
  :<|> handleUploadForm
  :<|> handleUpload

handleUploadForm :: Handler Lazy.ByteString
handleUploadForm = return $ renderHtml uploadForm

handleUpload :: EpisodeUpload -> Handler String
handleUpload EpisodeUpload{..} = do
  now <- liftIO getCurrentTime
  let rfc822 = formatTime defaultTimeLocale rfc822DateFormat now
      date = Text.pack $ formatTime defaultTimeLocale "%F" now
      filename = date <> "_" <> convertToFilename epTitle
      audioFile = filename <> Text.pack (takeExtensions epAudioFile)
      thumbnailFile = audioFile <> Text.pack (takeExtensions epThumbnailFile)

      episodeTitle = epTitle
      episodeAudioFile = Text.unpack audioFile
      episodeThumbnailFile = Text.unpack thumbnailFile
      episodeDescription = epDescription
      episode = Model.Episode{..}
  liftIO $ do
    copyFile epAudioFile episodeAudioFile
    copyFile epThumbnailFile episodeThumbnailFile

  return "Hi"

handleFeedXML :: Handler Lazy.ByteString
handleFeedXML = do
  let contents = renderMarkup (
        let title = "völlig irrelevant"
            link = "https://podcast.rubenmoor.net"
            img = "https://podcast.rubenmoor.net/microphone.jpg"
            description = "Wir reden hier über Themen"
            copyright = "Ruben Moor & Lucas Weiß"
            email = "ruben.moor@gmail.com (Ruben Moor)"
            pubDate = "Thu, 17 Dec 2020 02:00:00 GMT"
            latestDate = "Thu, 17 Dec 2020 02:00:00 GMT"
            itunesSubtitle = "Wir reden hier über Themen (Subtitle)"
            itunesSummary = "Wir reden hier über Themen (Summary)"
            itunesAuthors = "Luke & Rubm"
        in  $(compileHtmlFile "feed.xml.tpl"))
  return contents

main :: IO ()
main = do
  runSqlite "podcasts.db" $ runMigration migrateAll
  runSettings defaultSettings app

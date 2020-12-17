{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad                 (when, unless)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger          (NoLoggingT (..), runNoLoggingT)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource  (runResourceT)
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Foldable                 (foldl')
import           Data.Pool                     (Pool)
import           Data.Text                     (Text, replace, toUpper)
import qualified Data.Text                     as Text
import           Data.Text.IO                  (readFile)
import           Data.Time.Clock               (getCurrentTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime,
                                                rfc822DateFormat)
import           Data.Time.LocalTime           (getZonedTime)
import           Database.Persist              (insert)
import           Database.Persist.Sqlite       (SqlBackend, runMigration,
                                                runSqlPool, runSqlite,
                                                withSqlitePool)
import           Model                         (migrateAll)
import           Network.Wai.Handler.Warp      (defaultSettings, runSettings)
import           Servant                       ((:<|>) (..), Application,
                                                throwError, err400)
import qualified Servant
import           Servant.Multipart             (MultipartData, Tmp)
import           Servant.Server                (hoistServer, serve, ServerError (..))
import           System.Directory              (copyFile)
import           System.FilePath.Posix         (takeExtensions)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Renderer.Utf8      (renderMarkup)
import           Text.Heterocephalus

import           Api                           (EpisodeUpload (..), api)
import           Html                          (uploadForm)
import qualified Model

import           Prelude                       (FilePath, IO, String, flip,
                                                return, ($), (<>), (==), (/=))

type AppState = Pool SqlBackend

type Handler = ReaderT AppState Servant.Handler

app :: AppState -> Application
app state = serve api $ hoistServer api (flip runReaderT state) $
       handleFeedXML
  :<|> handleUploadForm
  :<|> handleUpload

handleUploadForm :: Handler Lazy.ByteString
handleUploadForm = return $ renderHtml uploadForm

handleUpload :: EpisodeUpload -> Handler String
handleUpload EpisodeUpload{..} = do
  now <- liftIO getCurrentTime
  when (epAudioFilename == "") $ throwError $ err400 { errBody = "audio file field mandatory" }
  let rfc822 = formatTime defaultTimeLocale rfc822DateFormat now
      date = Text.pack $ formatTime defaultTimeLocale "%F" now
      filename = date <> "_" <> convertToFilename (toUpper epTitle)
      audioFile = filename <> Text.pack (takeExtensions $ Text.unpack epAudioFilename)
      thumbnailFile = filename <> Text.pack (takeExtensions $ Text.unpack epThumbnailFilename)

      episodeTitle = epTitle
      episodeAudioFile = Text.unpack audioFile
      episodeThumbnailFile =
        if epThumbnailFilename /= "" then Text.unpack thumbnailFile
                                     else ""
      episodeDescription = epDescription
      episodeAudioContentType = epAudioContentType
      episodeSlug = "todo"
      episodeDuration = 100
      episodePubdate = now
      episode = Model.Episode{..}
  pool <- ask
  liftIO $ do
    copyFile epAudioFile episodeAudioFile
    unless (epThumbnailFilename == "") $ copyFile epThumbnailFile episodeThumbnailFile
    flip runSqlPool pool $ insert episode
  return $ Text.unpack epAudioContentType

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
main =
  runNoLoggingT $ withSqlitePool "podcasts.db" 10 $ \pool -> do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    NoLoggingT $ runSettings defaultSettings $ app pool

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

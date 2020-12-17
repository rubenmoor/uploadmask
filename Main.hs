{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import Options.Applicative
import           Control.Monad                 (unless, when)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Logger          (NoLoggingT (..), runNoLoggingT)
import           Control.Monad.Reader          (MonadReader)
import           Control.Monad.Trans.Reader    (Reader, ReaderT, ask,
                                                runReaderT)
import           Control.Monad.Trans.Resource  (runResourceT)
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Foldable                 (foldl')
import           Data.List                     (sortBy)
import           Data.Ord                      (Down (..), comparing)
import           Data.Pool                     (Pool)
import           Data.Text                     (Text, replace, toUpper)
import qualified Data.Text                     as Text
import           Data.Text.IO                  (readFile)
import           Data.Time.Clock               (getCurrentTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime,
                                                rfc822DateFormat)
import           Data.Time.LocalTime           (getZonedTime)
import           Database.Persist              (insert)
import           Database.Persist.Sqlite       (SqlBackend, SqlPersistT,
                                                runMigration, runSqlPool,
                                                runSqlite, withSqlitePool)
import           Model                         (Episode (..), episodePubdate,
                                                migrateAll)
import           Network.Wai.Handler.Warp      (defaultSettings, run)
import           Servant                       ((:<|>) (..), Application,
                                                err400, throwError)
import qualified Servant
import           Servant.Multipart             (MultipartData, Tmp)
import           Servant.Server                (ServerError (..), hoistServer,
                                                serve)
import           System.Directory              (copyFile)
import           System.FilePath.Posix         (takeExtensions, (</>))
import           System.Posix                  (fileSize, getFileStatus)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Blaze.Renderer.Utf8      (renderMarkup)
import           Text.Heterocephalus
import           Text.Printf                   (printf)

import           Api                           (EpisodeUpload (..), api)
import           Database.Gerippe              ((%), BaseBackend, Entity,
                                                IsPersistBackend,
                                                PersistEntityBackend, entityVal,
                                                getAll)
import           Html                          (uploadForm)
import qualified Model

import           Prelude                       ((*), (/), Int, FilePath, IO, String, flip,
                                                fromIntegral, map, putStrLn,
                                                return, ($), (.), (/=), (<$>),
                                                (<>), (==), mod, div)

type AppState = Pool SqlBackend

type Handler = ReaderT AppState Servant.Handler
mediaDir :: FilePath
mediaDir = "media"

app :: AppState -> Application
app state = serve api $ hoistServer api (flip runReaderT state) $
       handleFeedXML
  :<|> handleUploadForm
  :<|> handleUpload

handleUploadForm :: Handler Lazy.ByteString
handleUploadForm = return $ renderHtml uploadForm

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `div` 60) `mod` 60
      hours = d `mod` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds

handleUpload :: EpisodeUpload -> Handler String
handleUpload EpisodeUpload{..} = do
  now <- liftIO getCurrentTime
  when (uploadAudioFilename == "") $
    throwError $ err400 { errBody = "audio file field mandatory" }
  let rfc822 = formatTime defaultTimeLocale rfc822DateFormat now
      date = Text.pack $ formatTime defaultTimeLocale "%F" now
      slug = date <> "_" <> convertToFilename (toUpper uploadTitle)
      audioFile = slug <> Text.pack (takeExtensions $ Text.unpack uploadAudioFilename)

      -- fill Model.episode
      episodeTitle = uploadTitle
      episodeAudioFile = mediaDir </> Text.unpack audioFile
      episodeThumbnailFile =
        if uploadThumbnailFilename /= "\"\""
        then mediaDir </> Text.unpack (slug
               <> Text.pack (takeExtensions $ Text.unpack uploadThumbnailFilename))
        else ""
      episodeDescription = uploadDescription
      episodeAudioContentType = uploadAudioContentType
      episodeSlug = slug
      episodeDuration = uploadDuration
      episodeDurationColons = formatDuration uploadDuration
      episodeCreated = now
      episodePubdate = Text.pack rfc822
  episodeFileSize <- liftIO $ fromIntegral . fileSize <$> getFileStatus uploadAudioFile
  let episode = Model.Episode{..}
  pool <- ask
  liftIO $ do
    putStrLn $ "Copying " <> uploadAudioFile <> " to " <> episodeAudioFile
    copyFile uploadAudioFile episodeAudioFile
    unless (uploadThumbnailFilename == "\"\"") $ do
      putStrLn $ "Filename: " <> Text.unpack uploadThumbnailFilename <> ". Copying " <> uploadThumbnailFile <> " to " <> episodeThumbnailFile
      copyFile uploadThumbnailFile episodeThumbnailFile
    flip runSqlPool pool $ insert episode
  return $ Text.unpack uploadAudioContentType

runDb ::
   (MonadIO m, IsPersistBackend backend, BaseBackend backend ~ SqlBackend) =>
  ReaderT backend IO b -> ReaderT (Pool backend) m b
runDb action = do
  pool <- ask
  liftIO $ runSqlPool action pool

handleFeedXML :: Handler Lazy.ByteString
handleFeedXML = do
  episodeList <- map entityVal <$> runDb getAll
  let contents = renderMarkup (
        let title = "völlig irrelevant" :: Text
            mediaDir = mediaDir
            link = "https://podcast.rubenmoor.net" :: Text
            img = "media/microphone.jpg" :: Text
            description = "Wir reden hier über Themen" :: Text
            copyright = "Ruben Moor & Lucas Weiß" :: Text
            email = "ruben.moor@gmail.com (Ruben Moor)" :: Text
            pubDate = "Thu, 17 Dec 2020 02:00:00 GMT" :: Text
            -- TODO
            latestDate = "Thu, 17 Dec 2020 02:00:00 GMT" :: Text
            itunesSubtitle = "Wir reden hier über Themen (Subtitle)" :: Text
            itunesSummary = "Wir reden hier über Themen (Summary)" :: Text
            authors = "Luke & Rubm" :: Text
            defaultThumbnail = "media/microphone.jpg" :: Text
            episodes = sortBy (comparing $ Down . episodeCreated) episodeList
        in  $(compileHtmlFile "feed.xml.tpl"))
  return contents

data Options = Options
  { optPort :: Int
  }

optParse :: Parser Options
optParse = Options <$> option auto (
     long "port"
  <> short 'p'
  <> metavar "PORT"
  <> value 3000
  <> help "serve at specified port"
  <> showDefault
  )

main :: IO ()
main = do
  Options{..} <- execParser $ info (optParse <**> helper) (
       fullDesc
    <> progDesc "Welcome to the homepage server of the podcast project"
    )
  runNoLoggingT $ withSqlitePool "podcasts.db" 10 $ \pool -> do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    NoLoggingT $ run optPort $ app pool

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

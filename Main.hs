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

import           Control.Monad                 (unless, when)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Logger          (NoLoggingT (..), runNoLoggingT)
import           Control.Monad.Reader          (MonadReader)
import           Control.Monad.Trans.Reader    (Reader, ReaderT, ask,
                                                runReaderT)
import           Control.Monad.Trans.Resource  (runResourceT)
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Foldable                 (foldl')
import           Data.List                     (sortOn, sortBy)
import           Data.Ord                      (Down (..), comparing)
import           Data.Pool                     (Pool)
import           Data.Text                     (Text, replace, toUpper)
import qualified Data.Text                     as Text
import           Data.Text.IO                  (readFile)
import           Data.Time.Clock               (getCurrentTime)
import           Data.Time.Format              (parseTimeM, defaultTimeLocale, formatTime,
                                                rfc822DateFormat)
import           Data.Time.LocalTime           (getZonedTime)
import           Database.Persist              (insert)
import           Database.Persist.MySQL        (SqlBackend, SqlPersistT,
                                                mkMySQLConnectInfo,
                                                runMigration, runSqlPool,
                                                withMySQLPool)
import           Model                         (Episode (..), migrateAll)
import           Network.Wai.Handler.Warp      (defaultSettings, run)
import           Options.Applicative
import           Safe                          (headMay)
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
import           Database.Gerippe              (BaseBackend, Entity,
                                                IsPersistBackend,
                                                PersistEntityBackend, entityVal,
                                                getAll, (%))
import           Hosting                       (mediaDir, mediaLink, mkFileUrl,
                                                podcastLink, protocol)
import           Html                          (uploadForm, homepage)
import qualified Model

import           Prelude                       (maybe, Bool (..), FilePath, IO, Int, Maybe (..),
                                                String, div, flip, fromIntegral,
                                                map, mod, putStrLn, return,
                                                show, ($), (*), (.), (/), (/=),
                                                (<$>), (<>), (==))

type AppState = Pool SqlBackend

type Handler = ReaderT AppState Servant.Handler

app :: AppState -> Application
app state = serve api $ hoistServer api (flip runReaderT state) $
       handleFeedXML
  :<|> handleUploadForm
  :<|> handleUpload
  :<|> handleHomepage

handleHomepage :: Handler Lazy.ByteString
handleHomepage = do
  episodes <- map entityVal <$> runDb getAll
  pure $ renderHtml $ homepage episodes

handleUploadForm :: Handler Lazy.ByteString
handleUploadForm = do
  now <- liftIO getCurrentTime
  let today = Text.pack $ formatTime defaultTimeLocale "%F" now
  pure $ renderHtml $ uploadForm today

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `div` 60) `mod` 60
      hours = d `div` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds

handleUpload :: EpisodeUpload -> Handler String
handleUpload EpisodeUpload{..} = do
  now <- liftIO getCurrentTime
  -- TODO: these check don't have any effect
  when (uploadAudioFilename == "\"\"") $
    throwError $ err400 { errBody = "audio file field mandatory" }
  when (uploadTitle == "") $
    throwError $ err400 { errBody = "title field is mandatory" }
  episodePubdate <- case parseTimeM False defaultTimeLocale "%F" (Text.unpack uploadDate) of
    Just d -> pure d
    Nothing -> throwError $ err400 { errBody = "could not parse date" }
  let day = Text.pack $ formatTime defaultTimeLocale "%F" episodePubdate
      slug = day <> "_" <> convertToFilename (toUpper uploadTitle)
      episodeFtExtension = Text.pack $ takeExtensions $ Text.unpack uploadAudioFilename
      audioFile = mediaDir </> Text.unpack slug <> Text.unpack episodeFtExtension
      -- fill Model.episode
      episodeTitle = uploadTitle
      episodeThumbnailFile =
        if uploadThumbnailFilename /= "\"\""
        then mediaDir </> Text.unpack (slug
               <> Text.pack (takeExtensions $ Text.unpack uploadThumbnailFilename))
        else ""
      episodeDescription = uploadDescription
      episodeAudioContentType = uploadAudioContentType
      episodeSlug = slug
      episodeDuration = uploadDuration
      episodeCreated = now
  episodeFileSize <- liftIO $ fromIntegral . fileSize <$> getFileStatus uploadAudioFile
  let episode = Model.Episode{..}
  liftIO $ do
    putStrLn $ "Copying " <> uploadAudioFile <> " to " <> audioFile
    copyFile uploadAudioFile audioFile
    unless (uploadThumbnailFilename == "\"\"") $ do
      putStrLn $ "Filename: " <> Text.unpack uploadThumbnailFilename <> ". Copying " <> uploadThumbnailFile <> " to " <> episodeThumbnailFile
      copyFile uploadThumbnailFile episodeThumbnailFile
  runDb $ insert episode
  return $ Text.unpack uploadAudioContentType

data EpisodeFeedData = EpisodeFeedData
    { efdRFC822            :: Text
    , efdSlug              :: Text
    -- filetype extension (.m4a)
    , efdFtExtension       :: Text -- filetype extension (.m4a)
    , efdAudioFileUrl      :: Text
    , efdPageUrl           :: Text
    , efdTitle             :: Text
    , efdThumbnailFile     :: Text
    , efdDescription       :: Text
    , efdAudioContentType  :: Text
    , efdDurationSeconds   :: Int
    , efdDurationFormatted :: Text
    , efdFileSize          :: Int
    }

getEpisodeFeedData :: Model.Episode -> EpisodeFeedData
getEpisodeFeedData Model.Episode{..} =
  let efdRFC822 = replace "UTC" "UT" $
        Text.pack $ formatTime defaultTimeLocale rfc822DateFormat episodeCreated
      efdSlug = episodeSlug
      efdFtExtension = episodeFtExtension
      efdAudioFileUrl = mkFileUrl efdFtExtension efdSlug
      efdPageUrl = protocol <> podcastLink <> "/" <> efdSlug
      efdTitle = episodeTitle
      efdThumbnailFile = protocol <> mediaLink <> "/" <>
        if episodeThumbnailFile == ""
        then "microphone.jpg"
        else Text.pack episodeThumbnailFile
      efdDescription = episodeDescription
      efdAudioContentType = episodeAudioContentType
      efdDurationSeconds = episodeDuration
      efdDurationFormatted = formatDuration episodeDuration
      efdFileSize = episodeFileSize
  in  EpisodeFeedData{..}

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
            img = "microphone.jpg" :: Text
            imgUrl = protocol <> mediaLink <> "/" <> img
            description = "Wir reden hier über Themen" :: Text
            copyright = "Ruben Moor & Lucas Weiß" :: Text
            email = "ruben.moor@gmail.com (Ruben Moor)" :: Text
            pubDate = "Thu, 17 Dec 2020 02:00:00 GMT" :: Text
            itunesSubtitle = "Wir reden hier über Themen (Subtitle)" :: Text
            itunesSummary = "Wir reden hier über Themen (Summary)" :: Text
            authors = "Luke & Rubm" :: Text
            itunesOwnerNames = "Luke and Rubm" :: Text
            episodeData = getEpisodeFeedData <$>
              sortOn  (Down . episodeCreated) episodeList
            latestDate = maybe pubDate efdRFC822 $ headMay episodeData
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
  putStrLn $ "Serving at port " <> show optPort
  let connectInfo = mkMySQLConnectInfo
        "mysql"
        "podcast"
        "HwEGjT3hQhs2"
        "podcast"
  runNoLoggingT $ withMySQLPool connectInfo 10 $ \pool -> do
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

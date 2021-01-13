{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Control.Applicative           ((*>))
import           Control.Monad                 (unless, when)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Logger          (NoLoggingT (..), runNoLoggingT)
import           Control.Monad.Reader          (MonadReader)
import           Control.Monad.Trans.Reader    (Reader, ReaderT, asks,
                                                runReaderT)
import           Control.Monad.Trans.Resource  (runResourceT)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Foldable                 (foldl')
import           Data.List                     (length, sortBy, sortOn)
import           Data.Ord                      (Down (..), comparing)
import           Data.Pool                     (Pool)
import           Data.Text                     (Text, replace, toUpper, words)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Text.IO                  (readFile)
import           Data.Time.Clock               (getCurrentTime)
import           Data.Time.Format              (defaultTimeLocale, formatTime,
                                                parseTimeM, rfc822DateFormat)
import           Data.Time.LocalTime           (getZonedTime)
import           Database.Persist              (insert)
import           Database.Persist.MySQL        (MySQLConnectInfo, SqlBackend,
                                                SqlPersistT, mkMySQLConnectInfo,
                                                runSqlPool, withMySQLPool)
import qualified Database.Persist.MySQL        as MySQL
import           Model                         (Episode (..), migrateAll)
import           Network.Socket                (HostName)
import           Network.Wai.Handler.Warp      (defaultSettings, run)
import           Options.Applicative
import           Safe                          (headMay)
import           Servant                       ((:<|>) (..), Application,
                                                err400, err404, throwError)
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
import           TextShow                      (showt)

import           Api                           (EpisodeUpload (..), api)
import qualified Api                           (Order (..), SortBy (..))
import           Database.Gerippe              (BaseBackend, Entity (..),
                                                IsPersistBackend,
                                                PersistEntityBackend, asc, desc,
                                                entityVal, from, getAll,
                                                getAllValues, getBy, getWhere,
                                                keyToId, orderBy, select, (%),
                                                (^.))
import           Hosting                       (mediaLink, mkFileUrl,
                                                podcastLink, protocol)
import           Html                          (Order (..), SortBy (..))
import qualified Html
import qualified Model

import           Prelude                       (Bool (..), FilePath, IO, Int,
                                                Maybe (..), String, div, flip,
                                                fromIntegral, map, maybe, mod,
                                                putStrLn, return, show, ($),
                                                (*), (-), (.), (/), (/=), (<$>),
                                                (<>), (==), (>>=))

data AppConfig = AppConfig
    { cfgPool      :: Pool SqlBackend
    , cfgStaticLoc :: Text
    , cfgMediaDir  :: FilePath
    }

type Handler = ReaderT AppConfig Servant.Handler

app :: AppConfig -> Application
app state = serve api $ hoistServer api (flip runReaderT state) $
       handleFeedXML
  :<|> handleUploadForm
  :<|> handleUpload
  :<|> handleHomepage
  :<|> handleEpisode

handleEpisode :: Text -> Maybe Text -> Handler Lazy.ByteString
handleEpisode slug timeStamp = do
  staticLoc <- asks cfgStaticLoc
  runDb (getBy $ Model.UniqueSlug slug) >>= \case
    Just (Entity _ episode) -> pure $ renderHtml $ Html.episode staticLoc episode
    Nothing                 -> throwError $ err404 { errBody = "episode not found" }

handleHomepage :: Maybe Api.SortBy -> Maybe Api.Order -> Handler Lazy.ByteString
handleHomepage mSortBy mOrder = do
  let sortBy = case mSortBy of
        Just Api.SortByDate -> case mOrder of
          Just Api.OrderDescending -> SortByDate OrderDescending
          Just Api.OrderAscending  -> SortByDate OrderAscending
          Nothing                  -> SortByDate OrderAscending
        Nothing -> SortByDate OrderAscending
  staticLoc <- asks cfgStaticLoc
  episodes <- case sortBy of
    SortByDate order -> case order of
      OrderDescending ->
        runDb $ select $ from $ \e -> do
          orderBy [ desc (e ^. Model.EpisodePubdate )]
          pure e
      OrderAscending ->
        runDb $ select $ from $ \e -> do
          orderBy [ asc (e ^. Model.EpisodePubdate )]
          pure e
  pure $ renderHtml $ Html.homepage staticLoc (map entityVal episodes) sortBy

handleUploadForm :: Handler Lazy.ByteString
handleUploadForm = do
  staticLoc <- asks cfgStaticLoc
  now <- liftIO getCurrentTime
  let today = Text.pack $ formatTime defaultTimeLocale "%F" now
  currentIndex <- showt . length <$> (runDb getAll :: Handler [Entity Model.Episode])
  pure $ renderHtml $ Html.uploadForm staticLoc today currentIndex

handleUpload :: EpisodeUpload -> Handler String
handleUpload EpisodeUpload{..} = do
  mediaDir <- asks cfgMediaDir
  now <- liftIO getCurrentTime
  -- TODO: these check don't have any effect
  when (uploadAudioFilename == "\"\"") $
    throwError $ err400 { errBody = "audio file field mandatory" }
  when (uploadTitle == "") $
    throwError $ err400 { errBody = "title field is mandatory" }
  episodePubdate <- case parseTimeM False defaultTimeLocale "%F" (Text.unpack uploadDate) of
    Just d  -> pure d
    Nothing -> throwError $ err400 { errBody = "could not parse date" }
  let day = Text.pack $ formatTime defaultTimeLocale "%F" episodePubdate
      slug = day <> "_" <> convertToFilename (toUpper uploadTitle)
      episodeFtExtension = Text.pack $ takeExtensions $ Text.unpack uploadAudioFilename
      audioFile = mediaDir </> Text.unpack slug <> Text.unpack episodeFtExtension
      -- fill Model.episode
      episodeCustomIndex = uploadCustomIndex
      episodeTitle = uploadTitle
      episodeThumbnailFile =
        if uploadThumbnailFilename /= "\"\""
        then mediaDir </> Text.unpack (slug
               <> Text.pack (takeExtensions $ Text.unpack uploadThumbnailFilename))
        else ""
      episodeDescriptionShort = uploadDescription
      episodeDescriptionLong = uploadDescription
      episodeAudioContentType = uploadAudioContentType
      episodeSlug = slug
      episodeDuration = uploadDuration
      episodeCreated = now
      episodeVideoUrl = ""
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

getEpisodeFeedData :: Text -> Model.Episode -> EpisodeFeedData
getEpisodeFeedData staticLoc Model.Episode{..} =
  let efdRFC822 = replace "UTC" "UT" $
        Text.pack $ formatTime defaultTimeLocale rfc822DateFormat episodeCreated
      efdSlug = episodeSlug
      efdFtExtension = episodeFtExtension
      efdAudioFileUrl = mkFileUrl staticLoc efdFtExtension efdSlug
      efdPageUrl = protocol <> podcastLink <> "/" <> efdSlug
      efdTitle = episodeTitle
      efdThumbnailFile = mediaLink staticLoc <> "/" <>
        if episodeThumbnailFile == "" then "microphone.jpg"
        else Text.pack episodeThumbnailFile
      efdDescription = episodeDescriptionShort
      efdAudioContentType = episodeAudioContentType
      efdDurationSeconds = episodeDuration
      efdDurationFormatted = Html.formatDuration episodeDuration
      efdFileSize = episodeFileSize
  in  EpisodeFeedData{..}

runDb :: MonadIO m => ReaderT SqlBackend IO a -> ReaderT AppConfig m a
runDb action = do
  pool <- asks cfgPool
  liftIO $ runSqlPool action pool

handleFeedXML :: Handler Lazy.ByteString
handleFeedXML = do
  staticLoc <- asks cfgStaticLoc
  episodeList <- runDb getAllValues
  let contents = renderMarkup (
        let title = "völlig irrelevant" :: Text
            img = "microphone.jpg" :: Text
            imgUrl = mediaLink staticLoc <> "/" <> img
            description = "Wir reden hier über Themen" :: Text
            copyright = "Rubm & Luke" :: Text
            email = "luke.rubm@gmail.com (Luke & Rubm)" :: Text
            pubDate = "Thu, 17 Dec 2020 02:00:00 GMT" :: Text
            itunesSubtitle = "Wir reden hier über Themen (Subtitle)" :: Text
            itunesSummary = "Wir reden hier über Themen (Summary)" :: Text
            authors = "Luke & Rubm" :: Text
            itunesOwnerNames = "Luke and Rubm" :: Text
            episodeData = getEpisodeFeedData staticLoc <$>
              sortOn  (Down . episodeCreated) episodeList
            latestDate = maybe pubDate efdRFC822 $ headMay episodeData
        in  $(compileHtmlFile "feed.xml.tpl"))
  return contents

data Options = Options
    { optPort      :: Int
    , optStaticLoc :: Text
    , optMediaDir  :: FilePath
    , optHost      :: HostName
    , optUser      :: ByteString
    , optPwd       :: ByteString
    , optDbName    :: ByteString
    }

parseMyOptions :: Parser Options
parseMyOptions = Options
  <$> option auto (
           long "port"
        <> short 'p'
        <> metavar "PORT"
        <> value 3000
        <> help "serve at specified port"
        <> showDefault
        )
  <*> strOption (
            long "static-location"
         <> metavar "STATICLOC"
         <> value "http://localhost:3001/static"
         <> help "location of static files"
         <> showDefault
         )
  <*> strOption (
            long "media-directory"
        <> metavar "MEDIADIR"
        <> value "media"
        <> help "audio files will be copied here"
        <> showDefault
        )
  <*> strOption (
           long "mysql-host"
        <> metavar "HOST"
        <> value "localhost"
        <> help "mysql host"
        <> showDefault
        )
  <*> strOption (
           long "mysql-user"
        <> metavar "USER"
        <> value "podcast"
        <> help "mysql user"
        <> showDefault
        )
  <*> strOption (
           long "mysql-password"
        <> metavar "PASSWORD"
        <> value "foobar"
        <> help "mysql password for given user"
        )
  <*> strOption (
           long "mysql-database"
        <> metavar "DATABASE_NAME"
        <> value "podcast"
        <> help "mysql database name"
        <> showDefault
        )

main :: IO ()
main = do
  Options{..} <- execParser $ info (parseMyOptions <**> helper) (
       fullDesc
    <> progDesc "Welcome to the homepage server of the podcast project"
    )
  putStrLn $ "Serving at port " <> show optPort
  let connectInfo = mkMySQLConnectInfo optHost optUser optPwd optDbName
  runNoLoggingT $  withMySQLPool connectInfo 10 $ \pool -> do
    runResourceT $ flip runSqlPool pool $ MySQL.runMigration migrateAll
    let config = AppConfig
          { cfgPool = pool
          , cfgMediaDir = optMediaDir
          , cfgStaticLoc = optStaticLoc
          }
    NoLoggingT $ run optPort $ app config

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

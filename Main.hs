{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Api                      (api)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Text
import           Data.Text.IO             (readFile)
import qualified Data.ByteString.Lazy           as Lazy
import           Database.Persist
import           Database.Persist.Sqlite  (runMigration, runSqlite)
import           Model                    (migrateAll)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings)
import           Servant                  ((:<|>)(..), Application, Handler)
import           Servant.Multipart        (MultipartData, Tmp)
import           Servant.Server           (serve)
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Heterocephalus

import Html (uploadForm)

import           Prelude                  (FilePath, IO, String, return, ($))

app :: Application
app = serve api $
       handleFeedXML
  :<|> handleUploadForm
  :<|> handleUpload

handleUploadForm :: Handler Lazy.ByteString
handleUploadForm = return $ renderHtml uploadForm

handleUpload :: MultipartData Tmp -> Handler String
handleUpload multipartData = return "Hi"

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

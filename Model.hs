{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import Data.Text
import Data.Time (UTCTime)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
  title            Text
  slug             Text
  audioFile        FilePath
  audioContentType Text
  thumbnailFile    FilePath
  description      Text
  duration         Int           -- duration in seconds
  durationColons   Text
  fileSize         Int           -- file size in bytes
  created          UTCTime
  pubdate          Text          -- rfc822 formatted time
|]

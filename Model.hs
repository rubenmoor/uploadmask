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
import Data.Time.Calendar (Day)

import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
  title            Text
  slug             Text
  UniqueSlug slug
  customIndex      Text
  ftExtension      Text
  audioContentType Text
  thumbnailFile    FilePath
  descriptionShort Text
  descriptionLong  Text
  duration         Int           -- duration in seconds
  fileSize         Int           -- file size in bytes
  pubdate          Day           -- day of recording
  created          UTCTime
  videoUrl         Text
|]

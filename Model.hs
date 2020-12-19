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
import Database.Persist.MySQL
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
  title            Text
  slug             Text
  ftExtension      Text
  audioContentType Text
  thumbnailFile    FilePath
  description      Text
  duration         Int           -- duration in seconds
  fileSize         Int           -- file size in bytes
  created          UTCTime
|]

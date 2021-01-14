{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Hosting
  ( module Hosting
  ) where

import Data.Text (Text, breakOn, drop)

import Prelude (($), snd, (<>))

protocol :: Text
protocol = "https://"

podcastLink :: Text
podcastLink = "www.serendipity.works"

mediaLink :: Text -> Text
mediaLink staticLoc = staticLoc <> "/media"

-- https://dts.podtrac.com/redirect.m4a/podcast-static.rubenmoor.net/media/2020-11-15_BANANE.m4a
mkFileUrl :: Text -> Text -> Text -> Text
mkFileUrl staticLoc filetypeExtension slug =
  let mediaLink' = drop 3 $ snd $ breakOn "://" $ mediaLink staticLoc
  in     protocol
      <> "dts.podtrac.com/redirect"
      <> filetypeExtension
      <> "/" <> mediaLink' <> "/"
      <> slug <> filetypeExtension

schnackUrl :: Text
schnackUrl = "https://schnack.serendipity.works/embed.js"

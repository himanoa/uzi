module Data.Uzi
  ( timesId,
    timesName,
    TC.TimesChannel,
    TC.TimesName,
    OT.organizeTimes,
    OT.OrganizeTimesError (..),
  )
where

import RIO
import Data.Discord.ChannelId
import Data.Uzi.OrganizeTimes as OT
import Data.Uzi.TimesChannel qualified as TC

timesId ::
  (Functor f) =>
  (ChannelId -> f ChannelId) ->
  TC.TimesChannel ->
  f TC.TimesChannel
timesId = TC.id

timesName ::
  (Functor f) =>
  (TC.TimesName -> f TC.TimesName) ->
  TC.TimesChannel ->
  f TC.TimesChannel
timesName = TC.name

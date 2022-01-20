{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module MyLib (icsPrint) where

import Data.ByteString.Builder
       (Builder, byteString, hPutBuilder, lazyByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Default (def)
import Data.Foldable (for_, traverse_)
import qualified Data.Text.Lazy.Encoding as TLE
import System.IO (stdout)
import Text.ICalendar

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
       ( Day
       , LocalTime
       , TimeZone
       , defaultTimeLocale
       , formatTime
       , getTimeZone
       , iso8601DateFormat
       , utcToLocalTime
       )
import Doc

formatVCalendar :: [VCalendar] -> IO Doc -- Needs IO for timezone :/
formatVCalendar = foldMap formatCal
  where
    formatCal :: VCalendar -> IO Doc
    formatCal = foldMap formatEvent . vcEvents
    formatEvent :: VEvent -> IO Doc
    formatEvent ev =
      maybe mempty (\ts -> formatTStamp ts <> pure (line <> line)) (veDTStart ev) <>
      pure (maybe mempty formatDescription (veDescription ev) <> line <> line) <>
      pure ("Attendees: " <> nest 2 (foldMap (\a -> line <> formatAttendee a) (veAttendee ev)))
    -- This is the cause for IO :/
    formatLocalTime :: LocalTime -> Doc
    formatLocalTime = text . T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
    formatDay :: Day -> Doc
    formatDay = text . T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
    formatTStamp :: DTStart -> IO Doc
    formatTStamp = \case
      DTStartDateTime (FloatingDateTime time) _ -> pure $ formatLocalTime time
      DTStartDateTime (ZonedDateTime time _) _ -> pure $ formatLocalTime time
      -- -- This requires IO :/
      -- -- I can't take the tz early because the timezone depends on the actual
      -- -- time which I only know at the time of rendering
      DTStartDateTime (UTCDateTime time) _ -> fmap (\tz -> formatLocalTime (utcToLocalTime tz time)) (getTimeZone time)
      DTStartDate (Date day) _ -> pure $ formatDay day
    formatDescription :: Description -> Doc
    formatDescription = text . TL.toStrict . descriptionValue
    formatAttendee :: Attendee -> Doc
    formatAttendee = maybe mempty (prefix "- " . text . TL.toStrict) . attendeeCN
    prefix :: Doc -> Doc -> Doc
    prefix p x = p <> x
    bshow :: Show a => a -> Doc
    bshow = text . T.pack . show

icsPrint :: FilePath -> IO ()
icsPrint path =
  parseICalendarFile def path >>= \case
  Right (cals, _warnings) -> hPutBuilder stdout . render =<< formatVCalendar cals
  Left errs -> print errs

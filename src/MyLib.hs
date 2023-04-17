{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module MyLib (icsPrint) where

import Data.ByteString.Builder
       (hPutBuilder)
import Data.Default (def)
import System.IO (stdout)
import Text.ICalendar

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
       ( getTimeZone
       , utcToLocalTime
       )
import Doc
import Data.Time.Format.ISO8601 (iso8601Format, formatShow, ISO8601)

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
    formatISO8601 :: ISO8601 t => t -> Doc
    formatISO8601 = text . T.pack . formatShow iso8601Format
    formatTStamp :: DTStart -> IO Doc
    formatTStamp = \case
      DTStartDateTime (FloatingDateTime time) _ -> pure $ formatISO8601 time
      DTStartDateTime (ZonedDateTime time _) _ -> pure $ formatISO8601 time
      -- -- This requires IO :/
      -- -- I can't take the tz early because the timezone depends on the actual
      -- -- time which I only know at the time of rendering
      DTStartDateTime (UTCDateTime time) _ -> fmap (\tz -> formatISO8601 (utcToLocalTime tz time)) (getTimeZone time)
      DTStartDate (Date day) _ -> pure $ formatISO8601 day
    formatDescription :: Description -> Doc
    formatDescription = text . TL.toStrict . descriptionValue
    formatAttendee :: Attendee -> Doc
    formatAttendee = maybe mempty (prefix "- " . text . TL.toStrict) . attendeeCN

icsPrint :: FilePath -> IO ()
icsPrint path =
  parseICalendarFile def path >>= \case
  Right (cals, _warnings) -> hPutBuilder stdout . render =<< formatVCalendar cals
  Left errs -> print errs

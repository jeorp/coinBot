{-# LANGUAGE OverloadedStrings #-}
module Time where 
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.System
import Data.Time.Calendar.OrdinalDate

dayToString :: Day -> String
dayToString = filter (/= '-') . show

getToday :: IO Day
getToday = getCurrentTime >>= fmap localDay . utcToLocal

getNmonthAgo :: Int -> IO Day
getNmonthAgo n = do
  today <- getToday
  let (year, day) = toOrdinalDate today
      nMonthAgo = day - n*30 
  if nMonthAgo > 0
    then return $ fromOrdinalDate year nMonthAgo
    else if (year - 1) `mod` 4 == 0
          then return $ fromOrdinalDate (year - 1) (366 + nMonthAgo)
          else return $ fromOrdinalDate (year - 1) (365 + nMonthAgo)


utcToLocal :: UTCTime -> IO LocalTime 
utcToLocal = fmap zonedTimeToLocalTime . utcToLocalZonedTime

sysToLocal :: SystemTime -> IO LocalTime 
sysToLocal =  utcToLocal . systemToUTCTime


tokyoTimeZone :: TimeZone
tokyoTimeZone = minutesToTimeZone 540

utcToTokyoTime :: UTCTime -> ZonedTime
utcToTokyoTime = utcToZonedTime tokyoTimeZone

--"2022-01-04T14:00:29.203Z" -> like "2022-01-04 14:02:56.033454234 UTC"
parseUtc :: T.Text -> UTCTime 
parseUtc text = 
  let (day, remain) = T.break (=='T') text
      minutes = T.takeWhile (/= 'Z') $ T.drop 1 remain
    in (read. T.unpack) $ T.intercalate " " [day, minutes] <> " UTC"
      
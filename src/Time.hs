
module Time where 
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.System

dayToString :: Day -> String
dayToString = filter (/= '-') . show

getToday :: IO Day
getToday = getCurrentTime >>= fmap localDay . utcToLocal  

utcToLocal :: UTCTime -> IO LocalTime 
utcToLocal = fmap zonedTimeToLocalTime . utcToLocalZonedTime

sysToLocal :: SystemTime -> IO LocalTime 
sysToLocal =  utcToLocal . systemToUTCTime

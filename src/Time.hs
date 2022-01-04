
module Time where 
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.System

getToday :: IO Day
getToday = getCurrentTime >>= fmap localDay . utcToLocal  

utcToLocal :: UTCTime -> IO LocalTime 
utcToLocal = fmap zonedTimeToLocalTime . utcToLocalZonedTime

sysToLocal :: SystemTime -> IO LocalTime 
sysToLocal = undefined

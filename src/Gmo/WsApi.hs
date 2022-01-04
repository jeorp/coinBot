{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 

module Gmo.WsApi where
import Common
import Control.Lens

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import qualified Network.WebSockets  as WS
import qualified Wuss                as WSS (runSecureClient)
import qualified Data.ByteString.Lazy.Char8 as B

--makeLenses 

wsStream :: String -> B.ByteString  -> (B.ByteString  -> IO ()) -> WS.ClientApp ()
wsStream tag subscribe recv conn = do
    putStrLn $ "wsStream : Connecting " <> tag  
    WS.sendTextData conn subscribe
    putStrLn $ "Success connect " <> tag

    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        recv msg -- action ex. insert something into DB 

    getLine -- its action should be forever.. otherwise, this scope will soon be finished.
    WS.sendClose conn ("goodBye" :: B.ByteString)

getRateStream :: Coin -> (B.ByteString  -> IO ()) -> WS.ClientApp ()
getRateStream coin =
    let subscribe = "{ \"command\" : \"subscribe\", \"channel\": \"ticker\", \"symbol\": \"" <> B.pack (show coin) <> "\" }" 
    in wsStream (show coin <> "_Rate Server") subscribe

getOrderBookStream :: Coin -> (B.ByteString -> IO ()) -> WS.ClientApp ()
getOrderBookStream coin =
    let subscribe = "{ \"command\" : \"subscribe\", \"channel\": \"orderbooks\", \"symbol\": \""<> B.pack (show coin) <> "\" }"
    in wsStream (show coin <> "_OrderBooks Server") subscribe

runStreamWS :: WS.ClientApp () -> IO ()
runStreamWS app = withSocketsDo $ WSS.runSecureClient "api.coin.z.com" 443 "/ws/public/v1" app
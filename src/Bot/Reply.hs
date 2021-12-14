{-# LANGUAGE OverloadedStrings #-}  
{-# LANGUAGE TemplateHaskell #-}
module Bot.Reply where

import Common (Config(Config))
import Control.Monad (when, void)
import Control.Monad.Trans
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO
import Control.Lens

import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

import GetToken
makeLenses ''Config


-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
    config <- extract
    let discordToken = either (const "") (^. bot_token) config :: Text 
    userFacingError <- runDiscord $ def 
      { discordToken = discordToken, discordOnEvent = eventHandler }
    TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (1 * 10^6)
        void $ restCall (R.CreateMessage (messageChannel m) "Pong!")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageText
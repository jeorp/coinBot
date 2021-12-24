{-# LANGUAGE OverloadedStrings #-}  
{-# LANGUAGE TemplateHaskell #-}
module Bot.Reply where

import Common (Config(Config))
import Control.Monad (when, void)
import Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Lens

import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

import GetToken
makeLenses ''Config

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

broadcast :: IO ()
broadcast = do
    config <- extract
    let discordToken = either (const "") (^. bot_token) config :: T.Text 
    userFacingError <- runDiscord $ def 
      { discordToken = discordToken, discordOnEvent = handler }
    TIO.putStrLn userFacingError

handler :: Event -> DiscordHandler ()
handler (MessageCreate m) = do
  when (not (fromBot m)) (doBroad m)
  where
    doBroad :: Message -> DiscordHandler ()
    doBroad m = do
      void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
      threadDelay (1 * 10^6)
      void $ restCall (R.CreateMessage (messageChannel m) "Pong!")
handler _ = return ()
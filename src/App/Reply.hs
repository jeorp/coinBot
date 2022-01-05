{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE Rank2Types #-}

module App.Reply where

import Data.Extensible
import Control.Lens hiding ((:>))

import Common
import GetToken
import Command
import Record
import Gmo.ToRecord
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as B 
import qualified Data.Text.IO as TIO
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R


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
  unless (fromBot m) (doBroad m)
handler _ = return ()



doBroad :: Message -> DiscordHandler ()
doBroad m = do
  let input = T.unpack (messageText m)
      isCoinSelect = getLast $ lookupFromRegisteredA input (def :: Coin)
      isAllSelect = getLast $ lookupFromRegisteredA input (def :: ALL)
      isMarginSelect = getLast $ lookupFromRegisteredA input (def :: MarginC)
      isAssetsSelect = getLast $ lookupFromRegisteredA input (def :: AssetsC)

  when (isJust isCoinSelect) $ replyCoinRate $ fromMaybe def isCoinSelect 
  when (isJust isAllSelect) replyAllRates
  when (isJust isMarginSelect) replyMargin
  when (isJust isAssetsSelect) replyAssets
  where
    replyCoinRate :: Coin -> DiscordHandler ()
    replyCoinRate c = do
      rate <-liftIO $ extractRate $ Just c
      case rate of
        Just r -> do
          void $ do
            restCall (R.CreateMessage (messageChannel m) $ rateToText r)
        _ -> do
          void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
          threadDelay (1 * 10^6)
          void $ restCall (R.CreateMessage (messageChannel m) "Can not get rate .. ")
          
      return ()
    
    replyAllRates :: DiscordHandler ()
    replyAllRates = do
      rates <- liftIO extractRates
      if V.null rates
        then void $ restCall (R.CreateMessage (messageChannel m) "Can not get rates .. ")
        else V.mapM_ (restCall . R.CreateMessage (messageChannel m) . rateToText) rates

    replyMargin :: DiscordHandler ()
    replyMargin = do
      margin <- liftIO extractMargin
      case margin of
        Just ma -> do
          void $ do
            restCall (R.CreateMessage (messageChannel m) $ marginToText ma)
        _ -> do
          void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
          threadDelay (1 * 10^6)
          void $ restCall (R.CreateMessage (messageChannel m) "Can not get margin .. ")
      return ()

    replyAssets :: DiscordHandler ()
    replyAssets = do
      assets <- liftIO extractAssets 
      if V.null assets
        then void $ restCall (R.CreateMessage (messageChannel m) "Can not get rates .. ")
        else V.mapM_ (restCall . R.CreateMessage (messageChannel m) . assetsToText) assets
    
    uploadFile :: FilePath -> T.Text -> DiscordHandler ()
    uploadFile path text = do
      s <- liftIO $ B.readFile path
      void $ restCall (R.CreateMessageUploadFile (messageChannel m) text s)

    sendPic :: FilePath -> T.Text -> DiscordHandler ()
    sendPic path text = do
      s <- liftIO $ B.readFile path
      let img_ = def {createEmbedImage = Just $ CreateEmbedImageUpload s}
      void $ restCall (R.CreateMessageEmbed (messageChannel m) text img_)
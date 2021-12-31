{-# LANGUAGE OverloadedStrings #-}


module GetToken (extract) where
import Common (configFile, Config(..), baseUrl)

import Prelude hiding (readFile)
import Data.Text 
import Data.Char
import Data.Text.IO
import Data.Attoparsec.Text

extractText :: Parser Text 
extractText = takeTill isEndOfLine 

channellabel :: Parser Text
channellabel = "entry:" *> extractText

discordBotTokenlabel :: Parser Text
discordBotTokenlabel = "discord_bot_token:" *> extractText

gmoApiKeyLable :: Parser Text
gmoApiKeyLable = "gmo_api_key:" *> extractText

gmoApiSecretLable :: Parser Text
gmoApiSecretLable = "gmo_api_secret:" *> extractText


configParser :: Parser Config
configParser = Config <$>
            channellabel <* endOfLine <*>
            discordBotTokenlabel <* endOfLine <*>
            gmoApiKeyLable <* endOfLine <*>         
            gmoApiSecretLable


extract :: IO (Either String Config)
extract = parseOnly configParser <$> readFile configFile 

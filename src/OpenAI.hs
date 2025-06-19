{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OpenAI (getTarotInterpretation) where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Environment (lookupEnv)
import GHC.Generics (Generic)

-- Request structures
data Message = Message
  { role :: String
  , content :: String
  } deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

data ChatRequest = ChatRequest
  { model :: String
  , messages :: [Message]
  } deriving (Show, Generic)

instance ToJSON ChatRequest

-- Response structures
data Choice = Choice
  { message :: Message
  } deriving (Show, Generic)

data ChatResponse = ChatResponse
  { choices :: [Choice]
  } deriving (Show, Generic)

instance FromJSON Choice
instance FromJSON ChatResponse

-- Main function
getTarotInterpretation :: String -> IO (Either String String)
getTarotInterpretation userPrompt = do
  apiKey <- lookupEnv "OPENAI_API_KEY"
  case apiKey of
    Nothing -> return $ Left "OpenAI API key not found in .env"
    Just key -> do
      let messages = [ Message "system" "You are a mystical tarot reader.",
                       Message "user" userPrompt ]
          requestBody = encode $ ChatRequest "gpt-3.5-turbo" messages

      initialRequest <- parseRequest "https://api.openai.com/v1/chat/completions"
      let request = setRequestMethod "POST"
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ setRequestHeader "Authorization" [BS.pack $ "Bearer " ++ key]
                  $ setRequestBodyLBS requestBody
                  $ initialRequest

      response <- httpLBS request
      let body = getResponseBody response

      -- DEBUGGING HELP:
    --   putStrLn "\n=== RAW JSON RESPONSE ==="
    --   LBS.putStrLn body

      -- Try to parse only the message content
      case eitherDecode body :: Either String ChatResponse of
        Left err -> return $ Left ("JSON parse error: " ++ err)
        Right chat -> do
          let interpretation = content . message . head . choices $ chat
          return $ Right interpretation

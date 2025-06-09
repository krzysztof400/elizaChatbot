{-# LANGUAGE DeriveGeneric #-}

module Bot.Types where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

--import Bot.KnowledgeBase


-- | Fact that will be stored by the chatbot.
data Fact = Like T.Text
           | Dislike T.Text
           | Seen T.Text
           deriving (Show, Eq, Generic)

instance ToJSON Fact
instance FromJSON Fact


type UserInput = T.Text
-- | Keyword used for pattern matching.
type Keyword = T.Text

-- | Response from the bot
type Response = T.Text

-- | Responses connected to the keyword
-- | type KnowledgeBase = M.Map Keyword [Response]

-- | Bot Memory (facts about the user)
newtype BotMemory = BotMemory {
    facts :: [Fact]
} deriving (Show, Generic)

instance ToJSON BotMemory
instance FromJSON BotMemory


-- | Pattern type
data PatternRule = PatternRule {
    patternRegex :: String
    , response :: String
}
 
data Movie = Movie
  { title :: String
  , genres :: [String]
  , director :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Movie
instance FromJSON Movie



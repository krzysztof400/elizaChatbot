module Bot.Types where

import qualified Data.Map as M
import Data.Text as T
import Data.Aeson (ToJSON, FromJSON)

-- | Fact that will be stored by the chatbot.
data Fact = Likes T.Text
           | Dislikes T.Text
           | Seen T.Text
           deriving (Show, Eq)

instance ToJSON Fact
instance FromJSON Fact


type UserInput = T.Text
-- | Keyword used for pattern matching.
type Keyword = String

-- | Response from the bot
type Response = String

-- | Responses connected to the keyword
type KnowledgeBase = M.Map Keyword [Response]

-- | Bot Memory (facts about the user)
newtype BotMemory = BotMemory {
    facts :: [Fact]
} deriving (Show)

instance ToJSON BotMemory
instance FromJSON BotMemory


-- | Pattern type
data PatternRule = PatternRule {
    patternRegex :: String
    , response :: String
}

-- | Current Bot State
data BotState = BotState {
    memory :: BotMemory
    , kb ::KnowledgeBase
}




module Bot.Types where

import qualified Data.Map as M

-- | Fact that will be stored by the chatbot.
type Fact = String

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




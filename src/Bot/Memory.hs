module Bot.Memory () where

import Data.List (nub)
import Data.Text as T
import Bot.Types 

type UserInput = T.Text

-- |do zmiany
addFact :: UserInput -> BotMemory -> BotMemory
addFact f (BotMemory fs) = BotMemory ( nub (f : fs))

listFacts :: BotMemory -> [Fact]
listFacts (BotMemory fs) = fs

updateFacts :: BotMemory -> UserInput -> BotMemory
updateFacts (BotMemory fs) t = addFact t (BotMemory fs) 

-- | TODO: implement functions for Facts extraction from input (extractLikes, extractDislikes, extractIntrest ...)
-- | TODO: implement remove fact, has fact, search facts, update facts
-- | TODO: data serialization to Name.file (facts about current user)
-- | TODO: BotMemory should be Map keyword [Facts] instead of facts

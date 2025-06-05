module Bot.Memory where

import Data.List (nub)
import Data.Text as T
import Bot.Types 
import Data.Maybe (mapMaybe)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)

<<<<<<< HEAD
type UserInput = T.Text

addFact :: Fact -> BotMemory -> BotMemory
=======
-- |do zmiany
addFact :: UserInput -> BotMemory -> BotMemory
>>>>>>> c1eb11360ee056cb682ab3ec982d6e984512feae
addFact f (BotMemory fs) = BotMemory ( nub (f : fs))

listFacts :: BotMemory -> [Fact]
listFacts (BotMemory fs) = fs 

matchRegexGroups :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexGroups input regex = 
    let (matched, _, _, captures) = input =~ (T.unpack regex) :: (T.Text, T.Text, T.Text, [T.Text])
    in if T.null matched
        then Nothing
        else Just (matched : captures)

extractLikes :: UserInput -> [Fact]
extractLikes input = 
    let lowerInput = T.toLower input
        likePatterns =
            [ ("I like\\s+(.*?)", Like)
            , ("I love\\s+(.*?)", Like)
            , ("I really liked\\s+(.*?)", Like)
            , ("I enjoyed\\s+(.*?)", Like)    
            , ("I loved\\s+(.*?)", Like)
            , ("I liked\\s+(.*?)", Like)
            ]

        processLikeRule :: T.Text -> (T.Text, T.Text -> Fact) -> Maybe Fact
        processLikeRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> Just $factConstructor capture
                                _ -> Nothing

        extractedFromPatterns = mapMaybe (processLikeRule lowerInput) likePatterns
    in extractedFromPatterns

extractDislikes :: UserInput -> [Fact]
extractDislikes input = 
    let lowerInput = T.toLower input
        dislikePatterns =
            [ ("I don't like\\s+(.*?)", Dislike)
            , ("I do not like\\s+(.*?)", Dislike)
            , ("I dont like\\s+(.*?)", Dislike)
            , ("I hate\\s+(.*?)", Dislike)
            , ("I dislike\\s+(.*?)", Dislike)
            , ("I didnt enjoy\\s+(.*?)", Dislike)    
            , ("I did not enjoy\\s+(.*?)", Dislike)  
            , ("I didn't enjoy\\s+(.*?)", Dislike) 
            , ("I hated\\s+(.*?)", Dislike)
            , ("I disliked\\s+(.*?)", Dislike)
            , ("I didn't like\\s+(.*?)", Dislike)
            , ("I did not like\\s+(.*?)", Dislike)
            , ("I didnt like\\s+(.*?)", Dislike)
            ]

        processDislikeRule :: T.Text -> (T.Text, T.Text -> Fact) -> Maybe Fact
        processDislikeRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> Just $factConstructor capture
                                _ -> Nothing

    in extractedFromPatterns = mapMaybe (processDislikeRule lowerInput) dislikePatterns

extractSeen :: UserInput -> [Fact]
extractSeen input = 
    let lowerInput = T.toLower input
        seenPatterns =
            [ ("I have seen\\s+(.*?)", Seen)
            , ("I was watching\\s+(.*?)", Seen)
            ]

        processSeenRule :: T.Text -> (T.Text, T.Text -> Fact) -> Maybe Fact
        processSeenRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> Just $factConstructor capture
                                _ -> Nothing

    in extractedFromPatterns = mapMaybe (processSeenRule lowerInput) seenPatterns

extractFacts :: UserInput -> [Fact]
extractFacts input = 
    extractLikes input ++
    extractDislikes input ++
    extractSeen input

updateFacts :: BotMemory -> UserInput -> BotMemory
updateFacts botMemory input = 
    let
        extractedFacts = extractFacts input
        updateMemory = foldr addFact botMemory  extractedFacts
    in updateMemory

hasFact :: BotMemory -> Fact -> Bool
hasFact (BotMemory fs) f = f 'elem' fs

saveMemory :: FilePath -> BotMemory -> IO ()
saveMemory path memory = BL.writeFile path (encode memory)

loadMemory :: FilePath -> IO BotMemory
loadMemory path = do
        exists <- doesFileExist path
        if not exists
            then return (BotMemory [])
            else do
                content <- BL.readFile path
                case decode content of
                    Just memory -> return memory
                    Nothing     -> return (BotMemory [])
-- | TODO: implement remove fact, has fact, search facts, update facts
-- | TODO: data serialization to Name.file (facts about current user)

module Bot.Memory () where

import Data.List (nub)
import Data.Text as T
import Bot.Types 
import Bot.KnowledgeBase
import Data.Maybe (mapMaybe)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)

type UserInput = T.Text

addFact :: Fact -> BotMemory -> BotMemory
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

--- | RECOMMEND MOVIES FUNCTIONS

scoreMatch :: Text -> Text -> Int -> Int
scoreMatch keyword fact points =
    if keyword 'isInfixOf' fact then points else 0

scoreFact :: Movie -> Text -> Int
scoreFact movie fact =
    let factLower = toLower fact
        titleLower = toLower (title movie)
        genresLower = map toLower (genre movie)
        directorLower = toLower (director movie)
        titleScore = scoreMatch titleLower fact 1
        genreScore = sum [ scoreMatch gen factLower 2 | gen <- genresLower ]
        directorScore = scoreMatch directorLower fact 5 -- | we prioritize director over title or genre in recommend
    in titleScore + genreScore + directorScore

scoreFacts :: Movie -> [Text] -> Int
scoreFacts movie facts = sum $ map (scoreFact movie) facts

isSeen :: Movie -> [Text] -> Bool
isSeen movie seen =
        any (\seenFact ->
         let seenLower = toLower seen
             titleLower = toLower (title movie)
         in
           titleLower `isInfixOf` seenLower
      ) seenFacts

score :: Movie -> [Text] -> [Text] -> [Text] -> Int
score movie liked disliked seen =
    if isSeen movie seen 
        then 0
        else (scoreFact movie liked) - (scoreFacts movie disliked)

recommendedMovies :: Int -> BotState -> [Movie]
recommendedMovies n (BotState (BotMemory facts) kb) =
    let 
        liked = [toLower t | Like t <- facts]
        disliked = [toLower t | Dislike t <- facts]
        seen = [toLower t | Seen t <- facts]

        scored = [ (movie, (score movie liked disliked seen)) | movie <- movies]
        sorted = take n $ map fst $ reverse $ sortOn snd scored
    in sorted


similiarMovies :: Int -> BotState -> Movie -> [Movie]


-- | TODO: implement remove fact, has fact, search facts, update facts
-- | TODO: data serialization to Name.file (facts about current user)

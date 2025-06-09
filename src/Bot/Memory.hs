module Bot.Memory () where

import qualified Data.List as L
import qualified Data.Text as T
import Bot.Types 
import Bot.KnowledgeBase
import Data.Maybe (mapMaybe)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import Text.Regex.TDFA ((=~))

addFact :: Fact -> BotMemory -> BotMemory
addFact f (BotMemory fs) = BotMemory ( L.nub (f : fs))

listFacts :: BotMemory -> [Fact]
listFacts (BotMemory fs) = fs 

matchRegexGroups :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexGroups input regex =
    let (matched, _, _, captures) = T.unpack input =~ T.unpack regex :: (String, String, String, [String])
    in if null matched
        then Nothing
        else Just (map T.pack (matched : captures))


extractLikes :: UserInput -> [Fact]
extractLikes input = 
    let lowerInput = T.toLower input
        likePatterns =
            [ (T.pack "i like\\s+(.*)", Like)
            , (T.pack "i love\\s+(.*)", Like)
            , (T.pack "i really liked(.+)", Like)
            , (T.pack "i enjoyed\\s+(.*)", Like)    
            , (T.pack "i loved\\s+(.*)", Like)
            , (T.pack "i liked\\s+(.*)", Like)
            ]

        processLikeRule :: T.Text -> (T.Text, T.Text -> Fact) -> Maybe Fact
        processLikeRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> Just $ factConstructor capture
                _ -> Nothing

        extractedFromPatterns = mapMaybe (processLikeRule lowerInput) likePatterns
    in extractedFromPatterns

extractDislikes :: UserInput -> [Fact]
extractDislikes input = 
    let lowerInput = T.toLower input
        dislikePatterns =
            [ (T.pack "i don't like\\s+(.*)", Dislike)
            , (T.pack "i do not like\\s+(.*)", Dislike)
            , (T.pack "i dont like\\s+(.*)", Dislike)
            , (T.pack "i hate\\s+(.*)", Dislike)
            , (T.pack "i dislike\\s+(.*)", Dislike)
            , (T.pack "i didnt enjoy\\s+(.*)", Dislike)    
            , (T.pack "i did not enjoy\\s+(.*)", Dislike)  
            , (T.pack "i didn't enjoy\\s+(.*)", Dislike) 
            , (T.pack "i hated\\s+(.*)", Dislike)
            , (T.pack "i disliked\\s+(.*)", Dislike)
            , (T.pack "i didn't like\\s+(.*)", Dislike)
            , (T.pack "i did not like\\s+(.*)", Dislike)
            , (T.pack "i didnt like\\s+(.*)", Dislike)
            ]

        processDislikeRule :: T.Text -> (T.Text, T.Text -> Fact) -> Maybe Fact
        processDislikeRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> Just $ factConstructor capture
                _ -> Nothing

        extractedFromPatterns = mapMaybe (processDislikeRule lowerInput) dislikePatterns
    in extractedFromPatterns

extractSeen :: UserInput -> [Fact]
extractSeen input = 
    let lowerInput = T.toLower input
        seenPatterns =
            [ (T.pack "i have seen\\s+(.*)", Seen)
            , (T.pack "i was watching\\s+(.*)", Seen)
            ]

        processSeenRule :: T.Text -> (T.Text, T.Text -> Fact) -> Maybe Fact
        processSeenRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> Just $ factConstructor capture
                _ -> Nothing

        extractedFromPatterns = mapMaybe (processSeenRule lowerInput) seenPatterns
    in extractedFromPatterns

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
hasFact (BotMemory fs) f = f `elem` fs

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

scoreMatch :: T.Text -> T.Text -> Int -> Int
scoreMatch keyword fact points =
    if keyword `T.isInfixOf` fact then points else 0

scoreFact :: Movie -> T.Text -> Int
scoreFact movie fact =
    let factLower = T.toLower fact
        titleLower = ((T.toLower) . (T.pack)) (title movie)
        genresLower = map ((T.toLower) . (T.pack)) (genres movie)
        directorLower = ((T.toLower) . (T.pack)) (director movie)
        titleScore = scoreMatch titleLower fact 1
        genreScore = sum [ scoreMatch gen factLower 2 | gen <- genresLower ]
        directorScore = scoreMatch directorLower fact 5 -- | we prioritize director over title or genre in recommend
    in titleScore + genreScore + directorScore

scoreFacts :: Movie -> [T.Text] -> Int
scoreFacts movie facts = sum $ map (scoreFact movie) facts

isSeen :: Movie -> [T.Text] -> Bool
isSeen movie seen =
    let titleLower = T.toLower (T.pack (title movie))
    in any (\seenFact -> titleLower `T.isInfixOf` T.toLower seenFact) seen
    

score :: Movie -> [T.Text] -> [T.Text] -> [T.Text] -> Int
score movie liked disliked seen =
    if isSeen movie seen 
        then 0
        else (scoreFacts movie liked) - (scoreFacts movie disliked)

recommendedMovies :: Int -> BotMemory -> [Movie]
recommendedMovies n (BotMemory fs) =
    let 
        liked = [T.toLower t | Like t <- fs]
        disliked = [T.toLower t | Dislike t <- fs]
        seen = [T.toLower t | Seen t <- fs]

        scored = [ (movie, (score movie liked disliked seen)) | movie <- movies]
        sorted = take n $ map fst $ reverse $ L.sortOn snd scored
    in sorted


--similiarMovies :: Int -> BotState -> Movie -> [Movie]


-- | TODO: implement remove fact, has fact, search facts, update facts
-- | TODO: data serialization to Name.file (facts about current user)

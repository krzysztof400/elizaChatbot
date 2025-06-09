module Bot.Memory where

import qualified Data.List as L
import qualified Data.Text as T
import Bot.Types 
import Bot.KnowledgeBase
import Data.Maybe (mapMaybe)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)
import Text.Regex.TDFA ((=~))

-- | Add a fact to bot memory, ensuring no duplicates
addFact :: Fact -> BotMemory -> BotMemory
addFact f (BotMemory fs) = BotMemory (L.nub (f : fs))

-- | List all facts in memory
listFacts :: BotMemory -> [Fact]
listFacts (BotMemory fs) = fs 

-- | Match regex and extract groups
matchRegexGroups :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexGroups input regex =
    let (matched, _, _, captures) = T.unpack input =~ T.unpack regex :: (String, String, String, [String])
    in if null matched
        then Nothing
        else Just (map T.pack (matched : captures))

-- | Extract likes from user input
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
        processLikeRule inputText (pattern, factConstructor) = 
            case matchRegexGroups inputText pattern of
                Just (_:capture:_) -> Just $ factConstructor (T.strip capture)
                _ -> Nothing

        extractedFromPatterns = mapMaybe (processLikeRule lowerInput) likePatterns
    in extractedFromPatterns

-- | Extract dislikes from user input
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
        processDislikeRule inputText (pattern, factConstructor) = 
            case matchRegexGroups inputText pattern of
                Just (_:capture:_) -> Just $ factConstructor (T.strip capture)
                _ -> Nothing

        extractedFromPatterns = mapMaybe (processDislikeRule lowerInput) dislikePatterns
    in extractedFromPatterns

-- | Extract seen movies from user input
extractSeen :: UserInput -> [Fact]
extractSeen input = 
    let lowerInput = T.toLower input
        seenPatterns =
            [ (T.pack "i have seen\\s+(.*)", Seen)
            , (T.pack "i was watching\\s+(.*)", Seen)
            , (T.pack "i watched\\s+(.*)", Seen)
            , (T.pack "i saw\\s+(.*)", Seen)
            ]

        processSeenRule :: T.Text -> (T.Text, T.Text -> Fact) -> Maybe Fact
        processSeenRule inputText (pattern, factConstructor) = 
            case matchRegexGroups inputText pattern of
                Just (_:capture:_) -> Just $ factConstructor (T.strip capture)
                _ -> Nothing

        extractedFromPatterns = mapMaybe (processSeenRule lowerInput) seenPatterns
    in extractedFromPatterns

-- | Extract all facts from user input
extractFacts :: UserInput -> [Fact]
extractFacts input = 
    extractLikes input ++
    extractDislikes input ++
    extractSeen input

-- | Update bot memory with facts from user input
updateFacts :: BotMemory -> UserInput -> BotMemory
updateFacts botMemory input = 
    let extractedFacts = extractFacts input
        updateMemory = foldr addFact botMemory extractedFacts
    in updateMemory

-- | Check if bot memory contains a specific fact
hasFact :: BotMemory -> Fact -> Bool
hasFact (BotMemory fs) f = f `elem` fs

-- | Save memory to file
saveMemory :: FilePath -> BotMemory -> IO ()
saveMemory path memory = BL.writeFile path (encode memory)

-- | Load memory from file
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

-- MOVIE RECOMMENDATION FUNCTIONS

-- | Score a keyword match against fact text
scoreMatch :: T.Text -> T.Text -> Int -> Int
scoreMatch keyword fact points =
    if keyword `T.isInfixOf` fact then points else 0

-- | Score a movie against a single fact
scoreFact :: Movie -> T.Text -> Int
scoreFact movie fact =
    let factLower = T.toLower fact
        titleLower = T.toLower (T.pack (title movie))
        genresLower = map (T.toLower . T.pack) (genres movie)
        directorLower = T.toLower (T.pack (director movie))
        titleScore = scoreMatch titleLower factLower 3
        genreScore = sum [scoreMatch gen factLower 2 | gen <- genresLower]
        directorScore = scoreMatch directorLower factLower 5 -- Prioritize director matches
    in titleScore + genreScore + directorScore

-- | Score a movie against multiple facts
scoreFacts :: Movie -> [T.Text] -> Int
scoreFacts movie facts = sum $ map (scoreFact movie) facts

-- | Check if a movie has been seen
isSeen :: Movie -> [T.Text] -> Bool
isSeen movie seenList =
    let titleLower = T.toLower (T.pack (title movie))
    in any (\seenFact -> titleLower `T.isInfixOf` T.toLower seenFact) seenList

-- | Calculate overall score for a movie
score :: Movie -> [T.Text] -> [T.Text] -> [T.Text] -> Int
score movie liked disliked seenList =
    if isSeen movie seenList 
        then 0 -- Don't recommend already seen movies
        else (scoreFacts movie liked) - (scoreFacts movie disliked)

-- | Get top N recommended movies based on user preferences
recommendedMovies :: Int -> BotMemory -> [Movie]
recommendedMovies n (BotMemory fs) =
    let liked = [t | Like t <- fs]
        disliked = [t | Dislike t <- fs]
        seenList = [t | Seen t <- fs]
        
        scored = [(movie, score movie liked disliked seenList) | movie <- movies]
        -- Filter out movies with score <= 0 and sort by score descending
        filtered = filter ((> 0) . snd) scored
        sorted = take n $ map fst $ L.sortOn (negate . snd) filtered
    in sorted

-- | Get movies similar to a given movie (placeholder for future implementation)
-- similarMovies :: Int -> BotState -> Movie -> [Movie]
-- similarMovies n state targetMovie = []
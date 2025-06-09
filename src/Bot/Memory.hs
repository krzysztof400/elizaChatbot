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
import Debug.Trace (trace)

addFact :: Fact -> BotMemory -> BotMemory
addFact f (BotMemory fs) = BotMemory ( L.nub (f : fs))

listFacts :: BotMemory -> [Fact]
listFacts (BotMemory fs) = fs 

matchRegexGroups :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexGroups input regex =
    let (_, matched, _, captures) = (T.unpack input) =~ (T.unpack regex) :: (String, String, String, [String])
    in if null matched
        then Nothing
        else Just (map T.pack (matched : captures))


extractLikes :: UserInput -> [Fact]
extractLikes input = 
    let lowerInput = T.toLower input
        likePatterns =
            [ (T.pack "i like(.+)", Like)
            , (T.pack "i love(.+)", Like)
            , (T.pack "i really liked(.+)", Like)
            , (T.pack "i enjoyed(.*)", Like)    
            , (T.pack "i loved(.+)", Like)
            , (T.pack "i liked(.+)", Like)
            ]

        processLikeRule :: T.Text -> (T.Text, T.Text -> Fact) -> [Fact]
        processLikeRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> 
                    let cleaned = T.strip capture
                    in [factConstructor cleaned, Seen cleaned]
                _ -> []

        extractedFromPatterns = concatMap (processLikeRule lowerInput) likePatterns
    in extractedFromPatterns

extractDislikes :: UserInput -> [Fact]
extractDislikes input = 
    let lowerInput = T.toLower input
        dislikePatterns =
            [ (T.pack "i don't like(.*)", Dislike)
            , (T.pack "i do not like(.*)", Dislike)
            , (T.pack "i dont like(.*)", Dislike)
            , (T.pack "i hate(.*)", Dislike)
            , (T.pack "i dislike(.*)", Dislike)
            , (T.pack "i didnt enjoy(.*)", Dislike)    
            , (T.pack "i did not enjoy(.*)", Dislike)  
            , (T.pack "i didn't enjoy(.*)", Dislike) 
            , (T.pack "i hated(.*)", Dislike)
            , (T.pack "i disliked(.*)", Dislike)
            , (T.pack "i didn't like(.*)", Dislike)
            , (T.pack "i did not like(.*)", Dislike)
            , (T.pack "i didnt like(.*)", Dislike)
            ]

        processDislikeRule :: T.Text -> (T.Text, T.Text -> Fact) -> [Fact]
        processDislikeRule input (pattern, factConstructor) = 
            case matchRegexGroups input pattern of
                Just (_:capture:_) -> 
                    let cleaned = T.strip capture
                    in [factConstructor cleaned, Seen cleaned]
                _ -> []

        extractedFromPatterns = concatMap (processDislikeRule lowerInput) dislikePatterns
    in extractedFromPatterns

extractSeen :: UserInput -> [Fact]
extractSeen input = 
    let lowerInput = T.toLower input
        seenPatterns =
            [ (T.pack "i have seen(.*)", Seen)
            , (T.pack "i was watching(.*)", Seen)
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
        titleScore = scoreMatch titleLower factLower 1
        genreScore = sum [ scoreMatch gen factLower 2 | gen <- genresLower ]
        directorScore = scoreMatch directorLower factLower 5 -- | we prioritize director over title or genre in recommend
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
        filtered = filter (\(_, sc) -> sc > 0) scored
        sorted = take n $ map fst $ reverse $ L.sortOn snd filtered
    in sorted

-- | TODO: data serialization to Name.file (facts about current user)

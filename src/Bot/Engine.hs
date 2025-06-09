{-# LANGUAGE OverloadedStrings #-}

module Bot.Engine where

import Bot.Types
import Bot.KnowledgeBase
import Bot.Memory
import qualified Data.Text as T
import qualified Data.List as L (foldl')
import Text.Regex.TDFA
import System.Random (randomRIO)
import Bot.Pattern
import Text.Regex.TDFA ((=~), makeRegexOpts, defaultCompOpt, defaultExecOpt, Regex, matchAllText, MatchText)
import Data.Array ((!), bounds)
import Data.Char (isDigit)

-- | Match input against a single pattern rule
matchPattern :: UserInput -> PatternRule -> Maybe Response
matchPattern input (PatternRule pattern responseTemplate) =
    let inputStr = T.unpack $ T.toLower input
        -- Compile the regex from the pattern string.
        -- Explicitly specify the type of compiledRegex as 'Regex'
        compiledRegex :: Regex
        compiledRegex = makeRegexOpts defaultCompOpt defaultExecOpt pattern
    in case matchM compiledRegex inputStr :: Maybe MatchArray of
        Just ma ->
            let response = substituteGroups responseTemplate inputStr ma
            in Just (T.pack response)
        Nothing -> Nothing

-- | Substitute captured groups into the response template.
-- Placeholders like $0, $1, $2, ... are replaced with the corresponding
-- captured group from the regex match. $0 is the whole match, $1 is the
-- first group, etc.
substituteGroups :: String            -- ^ The template string
                 -> String            -- ^ The original input string that was matched
                 -> MatchArray        -- ^ The array of match results (offsets and lengths)
                 -> String
substituteGroups template inputStr ma = go template
  where
    go [] = []
    go ('$':d:rest) | isDigit d =
        let groupIndex = read [d] :: Int
        in if groupIndex >= fst (bounds ma) && groupIndex <= snd (bounds ma)
           then
             let (offset, len) = ma ! groupIndex
             in if offset == -1 -- Check for non-participating group (e.g. optional group that didn't match)
                then "" ++ go rest -- Replace with empty string
                else take len (drop offset inputStr) ++ go rest
           else '$' : d : go rest -- Not a valid group index (e.g. $9 when only 2 groups), treat '$' and digit literally
    go (c:cs) = c : go cs

-- | Replace all occurrences of a substring
replaceAll :: String -> String -> String -> String
replaceAll old new = T.unpack . T.replace (T.pack old) (T.pack new) . T.pack

-- | Find first matching pattern and generate response
findMatchingResponse :: UserInput -> [PatternRule] -> Maybe Response
findMatchingResponse input patterns =
    case [resp | pattern <- patterns, Just resp <- [matchPattern input pattern]] of
        (response:_) -> Just response
        [] -> Nothing

-- | Get a random movie-focused default response
getMovieDefaultResponse :: IO T.Text
getMovieDefaultResponse = do
    idx <- randomRIO (0, length movieDefaultResponses - 1)
    return $ movieDefaultResponses !! idx

-- | Check if user input contains memory and acknowledge it
hasMovieMemory :: BotMemory -> UserInput -> Maybe T.Text
hasMovieMemory (BotMemory facts) input =
    let relevantFacts = filter (isMovieRelatedFact input) facts
    in if null relevantFacts
       then Nothing
       else Just $ "I remember you mentioned " <> formatMovieFacts relevantFacts

-- | Check if a fact relates to movies or entertainment
isMovieRelatedFact :: T.Text -> Fact -> Bool
isMovieRelatedFact input fact =
    let factText = getFactText fact
        -- Expanded keywords for better detection
        movieKeywords = ["movie", "film", "cinema", "theater", "theatre", "watch", "netflix", "streaming", "director", "actor", "actress", "genre", "plot", "scene", "character", "series", "sequel", "soundtrack", "review"]
        inputLower = T.toLower input
        factLower = T.toLower factText

        -- True if the fact's content itself is about movies
        factItselfIsMovieRelated = any (`T.isInfixOf` factLower) movieKeywords
        
        -- True if the current user input is about movies
        inputIsMovieRelated = any (`T.isInfixOf` inputLower) movieKeywords
        
        -- True if the fact's subject (e.g., a movie title) is mentioned in the current input
        factSubjectMentionedInInput = not (T.null factLower) && factLower `T.isInfixOf` inputLower

    in factItselfIsMovieRelated || (inputIsMovieRelated && factSubjectMentionedInInput)

-- | Extract text from fact
getFactText :: Fact -> T.Text
getFactText (Like thing) = thing
getFactText (Dislike thing) = thing
getFactText (Seen thing) = thing

-- | Format movie-related facts for display
formatMovieFacts :: [Fact] -> T.Text
formatMovieFacts facts = T.intercalate ", " (map formatMovieFact facts)

-- | Format a single movie-related fact
formatMovieFact :: Fact -> T.Text
formatMovieFact (Like thing) = "enjoying " <> thing
formatMovieFact (Dislike thing) = "not liking " <> thing
formatMovieFact (Seen thing) = "watching " <> thing

-- | Check if input is asking for recommendations
isRecommendationRequest :: UserInput -> Bool
isRecommendationRequest input =
    let lowerInput = T.toLower input
        recommendKeywords = ["recommend", "suggest", "what should i watch", "movie suggestion", "good movie"]
    in any (`T.isInfixOf` lowerInput) recommendKeywords

-- | Generate movie recommendations response
generateRecommendations :: BotMemory -> T.Text
generateRecommendations memory =
    let recommendations = recommendedMovies 3 memory
    in if null recommendations
       then "I'd love to recommend movies, but I need to know more about your preferences! Tell me about movies you like or dislike."
       else "Based on your preferences, I recommend: " <> 
            T.intercalate ", " (map (T.pack . title) recommendations) <> 
            ". Would you like to know more about any of these?"

-- | Main respond function - movie-focused version
respond :: BotState -> UserInput -> IO T.Text
respond (BotState memory _) input
    | T.null (T.strip input) = return "What movie would you like to talk about?"
    | isRecommendationRequest input = return $ generateRecommendations memory
    | otherwise = do
        let baseResponse = findMatchingResponse input moviePatterns
            memoryResponse = hasMovieMemory memory input
        
        finalBaseResponse <- case baseResponse of
            Just resp -> return resp
            Nothing -> getMovieDefaultResponse
        
        return $ case memoryResponse of
            Just memResp -> memResp <> T.pack ". " <> finalBaseResponse
            Nothing -> finalBaseResponse
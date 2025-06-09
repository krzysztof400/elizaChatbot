{-# LANGUAGE OverloadedStrings #-}

module Bot.Engine where

import Bot.Types
import Bot.KnowledgeBase
import Bot.Memory
import qualified Data.Text as T
import qualified Data.List as L
import Text.Regex.TDFA
import System.Random (randomRIO)
import Bot.Pattern

-- | Match input against a single pattern rule
matchPattern :: UserInput -> PatternRule -> Maybe Response
matchPattern input (PatternRule pattern responseTemplate) =
    let inputStr = T.unpack $ T.toLower input
        regex = makeRegexOpts defaultCompOpt defaultExecOpt pattern :: Regex
    in case matchM regex inputStr of
        Just match ->
            let groups = mrSubList match
                response = if length groups > 1
                          then substituteGroups responseTemplate (tail groups)
                          else responseTemplate
            in Just (T.pack response)
        Nothing -> Nothing

-- | Substitute captured groups in response template
substituteGroups :: String -> [String] -> String
substituteGroups template groups =
    T.unpack $ foldl (\acc (i, group) ->
        let placeholder = T.pack $ "$" ++ show i
            groupText = T.pack group
        in T.replace placeholder groupText acc
    ) (T.pack template) (zip [1..] groups)

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
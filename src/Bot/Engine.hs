module Engine where

import Bot.Types
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (toLower)
import Text.Regex.TDFA
import System.Random

-- Match input against a single pattern rule
matchPattern :: UserInput -> PatternRule -> Maybe Response
matchPattern input (PatternRule pattern responseTemplate) =
    let inputStr = T.unpack $ T.toLower input
        regex = makeRegexOpts defaultCompOpt defaultExecOpt pattern
    in case matchM regex inputStr of
        Just match -> 
            let groups = mrSubList match
                response = if length groups > 1 
                          then substituteGroups responseTemplate (tail groups)
                          else responseTemplate
            in Just (T.pack response)
        Nothing -> Nothing

-- Substitute captured groups in response template
substituteGroups :: String -> [String] -> String
substituteGroups template groups = 
    foldl (\acc (i, group) -> 
        let placeholder = "$" ++ show i
        in replaceAll placeholder group acc
    ) template (zip [1..] groups)

-- Replace all occurrences of a substring
replaceAll :: String -> String -> String -> String
replaceAll old new = T.unpack . T.replace (T.pack old) (T.pack new) . T.pack

-- Find first matching pattern and generate response
findMatchingResponse :: UserInput -> [PatternRule] -> Maybe Response
findMatchingResponse input patterns = 
    case [resp | pattern <- patterns, Just resp <- [matchPattern input pattern]] of
        (response:_) -> Just response
        [] -> Nothing

-- Get a movie-focused default response
getMovieDefaultResponse :: T.Text
getMovieDefaultResponse = head movieDefaultResponses

-- Check if user input contains memory and acknowledge it
hasMovieMemory :: BotMemory -> UserInput -> Maybe T.Text
hasMovieMemory (BotMemory facts) _ =
    if null facts
    then Nothing
    else Just $ "I remember you mentioned " <> formatMovieFacts facts

-- Check if a fact relates to movies or entertainment
-- isMovieRelatedFact :: T.Text -> Fact -> Bool
-- isMovieRelatedFact input fact = 
--     let factText = getFactText fact
--         movieKeywords = ["movie", "film", "cinema", "theater", "watch", "netflix", "streaming"]
--         hasMovieKeyword = any (`T.isInfixOf` factText) movieKeywords
--         isInInput = factText `T.isInfixOf` input
--     in hasMovieKeyword || isInInput

-- Extract text from fact
getFactText :: Fact -> T.Text
getFactText (Likes thing) = thing
getFactText (Dislikes thing) = thing
getFactText (Seen thing) = thing

-- Format movie-related facts for display
formatMovieFacts :: [Fact] -> T.Text
formatMovieFacts facts = T.intercalate ", " (map formatMovieFact facts)

-- Format a single movie-related fact
formatMovieFact :: Fact -> T.Text
formatMovieFact (Likes thing) = "enjoying " <> thing
formatMovieFact (Dislikes thing) = "not liking " <> thing
formatMovieFact (Seen thing) = "watching " <> thing

-- Main respond function - movie-focused version
respond :: BotState -> UserInput -> T.Text
respond (BotState memory _) input 
    | T.null (T.strip input) = "What movie would you like to talk about?"
    | otherwise = 
        let baseResponse = case findMatchingResponse input moviePatterns of
                Just resp -> resp
                Nothing -> getMovieDefaultResponse
        --     memoryResponse = hasMovieMemory memory input
        -- in case memoryResponse of
        --     Just memResp -> memResp <> ". " <> baseResponse
        --     Nothing -> baseResponse
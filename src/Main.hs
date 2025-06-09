{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Control.Monad (void)
import Bot.Types
import Bot.Memory
import Engine

updateInfo :: BotState -> UserInput -> BotState
updateInfo (BotState memory kb) input = BotState (addFact input memory) kb

conversation :: BotState -> IO BotState
conversation state = do
  putStr "You: "
  hFlush stdout
  line <- TIO.getLine
  let state' = updateInfo state line
      reply = respond state' line
  TIO.putStrLn $ "Eliza: " <> reply
  if any (`T.isInfixOf` T.toLower line) ["bye", "exit"]
    then return state'
    else conversation state'

main :: IO ()
main = do
    putStrLn "Hello traveler! This is Eliza program."
    putStrLn "I am a simple chatbot and you OUGHT TO ONLY SPEAK TO ME IN ENGLISH AND ONLY IN ENGLISH!"
    putStrLn "I will not understand anything else."
    let initialState = BotState { memory = BotMemory [], kb = undefined }
    void $ conversation initialState
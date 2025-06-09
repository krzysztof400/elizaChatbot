{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Control.Monad (void)
import Bot.Types
import Bot.Memory
import Bot.Engine
import Bot.KnowledgeBase

conversation :: BotMemory -> IO BotMemory
conversation memory = do
  putStr "You: "
  hFlush stdout
  line <- TIO.getLine
  let memory' = updateFacts memory line
  replyText <- respond memory' line
  TIO.putStrLn $ "Eliza: " <> replyText
  if any (`T.isInfixOf` T.toLower line) ["bye", "exit"]
    then return memory'
    else conversation memory'

main :: IO ()
main = do
    putStrLn "Hello traveler! This is Eliza program."
    putStrLn "I am a simple chatbot and you OUGHT TO ONLY SPEAK TO ME IN ENGLISH AND ONLY IN ENGLISH!"
    putStrLn "I will not understand anything else."
    let initialMemory = BotMemory []
    void $ conversation initialMemory
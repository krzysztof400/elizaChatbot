{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Control.Monad (void)

type BotMemory = T.Text
type UserInfo = T.Text

updateInfo :: BotMemory -> T.Text -> BotMemory
updateInfo memory input = memory <> "\n" <> input

respond :: BotMemory -> T.Text -> T.Text
respond _ input
  | "movie" `T.isInfixOf` T.toLower input = "Tell me more about the movies you like!"
  | otherwise = "I'm here to listen. Please continue."

conversation :: BotMemory -> IO UserInfo
conversation ui = do
  putStr "You: "
  hFlush stdout
  line <- TIO.getLine
  let ui' = updateInfo ui line
      reply = respond ui' line
  TIO.putStrLn $ "Eliza: " <> reply
  if any (`T.isInfixOf` T.toLower line) ["bye", "exit"]
    then return ui'
    else conversation ui'

main :: IO ()
main = do
    putStrLn "Hello traveler! This is Eliza program."
    putStrLn "I am a simple chatbot and you OUGHT TO ONLY SPEAK TO ME IN ENGLISH AND ONLY IN ENGLISH!"
    putStrLn "I will not understand anything else."
    void $ conversation ""
module Main where

import System.IO (hFlush, stdout)
import Spread
import OpenAI (getTarotInterpretation)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Char (toLower)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Random (randomRIO)
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.ANSI

-- Prompt helper
prompt :: String -> IO String
prompt text = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr (text ++ " ")
  hFlush stdout
  setSGR [Reset]
  getLine

putStrWithStyle :: Color -> String -> IO ()
putStrWithStyle color msg = do
  setSGR [SetColor Foreground Vivid color]
  putStrLn msg
  setSGR [Reset]

-- Session metadata
generateSessionInfo :: IO (String, String)
generateSessionInfo = do
  now <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" now
  sessionId <- fmap (printf "sess-%04X") (randomRIO (0, 0xFFFF) :: IO Int)
  return (timestamp, sessionId)

-- CLI Loop
main :: IO ()
main = do
  _ <- loadFile defaultConfig
  sessionLoop

sessionLoop :: IO ()
sessionLoop = do
  putStrLn ""
  putStrWithStyle Green "🔮 Welcome to Arcana Engine: CLI Tarot Reader"
  putStrLn ""
  putStrWithStyle Cyan "Choose your spread:"
  putStrLn "1. 3-card   (Past / Present / Future)"
  putStrLn "2. celtic   (10-card situational spread)"
  putStrLn "3. shadow   (5-card inner healing)"
  putStrLn "4. venture  (5-card new beginning spread)"
  putStrLn "5. intuitive (pull as many cards as you feel called to — up to 15)"
  putStrLn ""

  choice <- prompt "Enter spread type [3-card, celtic, shadow, venture, intuitive]:"
  let normalizedChoice = map toLower choice

  printSpreadShape normalizedChoice
  putStrLn ""

  case normalizedChoice of
    "3-card"    -> handleThreeCard
    "celtic"    -> handleCeltic
    "shadow"    -> handleShadow
    "venture"   -> handleVenture
    "intuitive" -> handleIntuitive
    _           -> putStrWithStyle Red "⚠️ Invalid spread type. Please try again."

  again <- prompt "\nWould you like to do another spread? (y/n):"
  if map toLower again `elem` ["y", "yes"]
    then sessionLoop
    else putStrWithStyle Magenta "\n🌙 Thank you for using Arcana Engine. May the cards guide you well."

-- Input + format helpers
getCardInput :: String -> IO String
getCardInput position = do
  card <- prompt $ "Enter the card for \"" ++ position ++ "\":"
  reversed <- prompt "Is this card reversed? (y/n):"
  let isReversed = map toLower reversed `elem` ["y", "yes"]
  return $ if isReversed then card ++ " ⬇️ (reversed)" else card

-- Spread shapes
printSpreadShape :: String -> IO ()
printSpreadShape spread = case map toLower spread of
  "3-card" -> putStrLn "\n  [Past]   [Present]   [Future]\n"
  "celtic" -> putStrLn "\n      [5]\n  [3] [1]/[2] [4]\n      [6]\n  [7] [8] [9] [10]\n"
  "shadow" -> putStrLn "\n  [1] Shadow Self\n  [2] Root Cause\n  [3] Effect\n  [4] Healing\n  [5] Integration\n"
  "venture" -> putStrLn "\n  [1] Seed\n  [2] Soil\n  [3] Shadows\n  [4] Light\n  [5] Fruit\n"
  _ -> return ()

-- Spread Handlers
handleThreeCard :: IO ()
handleThreeCard = do
  putStrLn "\n🔹 3-Card Spread"
  past <- getCardInput "Past"
  present <- getCardInput "Present"
  future <- getCardInput "Future"
  runPrompt $ unlines
    [ "You are a mystical tarot interpreter.",
      "Interpret the following 3-card spread:",
      "Past: " ++ past,
      "Present: " ++ present,
      "Future: " ++ future,
      "Respond with information regarding each card and how it fits into the whole picture of the tarot pull. Describe each card in terms of what is depicted on the front of each card according to the Rider Waite tarot deck illustrations. Additionally, provide a culminating synthesis for the reading as well. At the end, include affirmations from the reading."
    ]

handleCeltic :: IO ()
handleCeltic = do
  putStrLn "\n🔹 Celtic Cross Spread"
  cards <- mapM getCardInput
    [ "1. Present", "2. Challenge", "3. Past", "4. Future",
      "5. Conscious (Above)", "6. Subconscious (Below)",
      "7. Advice", "8. External Influence",
      "9. Hopes and Fears", "10. Outcome" ]
  let promptText = unlines $
        [ "You are a wise tarot guide.",
          "Interpret this 10-card Celtic Cross spread in depth:" ] ++
        zipWith (\label card -> label ++ ": " ++ card)
          [ "Present", "Challenge", "Past", "Future", "Conscious", "Subconscious",
            "Advice", "External", "Hopes/Fears", "Outcome" ] cards ++
        [ "Respond with information regarding each card and how it fits into the whole picture of the tarot pull. Describe each card in terms of what is depicted on the front of each card according to the Rider Waite tarot deck illustrations. Additionally, provide a culminating synthesis for the reading as well. At the end, include affirmations from the reading." ]
  runPrompt promptText

handleShadow :: IO ()
handleShadow = do
  putStrLn "\n🔹 Shadow Work Spread"
  cards <- mapM getCardInput
    [ "1. Shadow Self", "2. Root of the Shadow", "3. How It Affects You", "4. What Needs Healing", "5. Integration" ]
  let promptText = unlines $
        [ "You are a mystical guide for inner healing.",
          "Interpret this shadow work tarot spread with emotional depth:" ] ++
        zipWith (\label card -> label ++ ": " ++ card)
          [ "Shadow Self", "Root Cause", "Effects", "Healing Path", "Integration" ] cards ++
        [ "Respond with information regarding each card and how it fits into the whole picture of the tarot pull. Describe each card in terms of what is depicted on the front of each card according to the Rider Waite tarot deck illustrations. Additionally, provide a culminating synthesis for the reading as well. At the end, include affirmations from the reading." ]
  runPrompt promptText

handleVenture :: IO ()
handleVenture = do
  putStrLn "\n🔹 New Venture Spread"
  cards <- mapM getCardInput
    [ "1. The Seed (idea or intention)", "2. The Soil (support and resources)",
      "3. The Shadows (challenges or fears)", "4. The Light (strengths you bring)",
      "5. The Fruit (potential outcome)" ]
  let promptText = unlines $
        [ "You are a spiritual guide for new beginnings.",
          "Interpret this tarot spread for someone starting a new venture:" ] ++
        zipWith (\label card -> label ++ ": " ++ card)
          [ "Seed", "Soil", "Shadows", "Light", "Fruit" ] cards ++
        [ "Respond with information regarding each card and how it fits into the whole picture of the tarot pull. Describe each card in terms of what is depicted on the front of each card according to the Rider Waite tarot deck illustrations. Additionally, provide a culminating synthesis for the reading as well. At the end, include affirmations from the reading." ]
  runPrompt promptText

handleIntuitive :: IO ()
handleIntuitive = do
  putStrLn "\n🌌 Intuitive Spread: Pull as many cards as you feel called to (up to 15)."
  pullLoop 1 []

pullLoop :: Int -> [(String, String)] -> IO ()
pullLoop n acc
  | n > 15 = finalizeIntuitive acc
  | otherwise = do
      label <- prompt $ "Label for card #" ++ show n ++ " (e.g., 'Insight', 'Guidance', or just leave blank):"
      card  <- prompt $ "Enter the card name for card #" ++ show n ++ ":"
      reversed <- prompt "Is this card reversed? (y/n):"
      let isReversed = map toLower reversed `elem` ["y", "yes"]
          fullCard = if isReversed then card ++ " ⬇️ (reversed)" else card
          cardLabel = if null label then "Card #" ++ show n else label
      another <- prompt "Pull another card? (y/n):"
      let updated = acc ++ [(cardLabel, fullCard)]
      if map toLower another `elem` ["y", "yes"]
        then pullLoop (n + 1) updated
        else finalizeIntuitive updated

finalizeIntuitive :: [(String, String)] -> IO ()
finalizeIntuitive labeledCards = do
  putStrLn "\n🃏 Your intuitive spread:"
  mapM_ (\(label, card) -> putStrLn $ "• " ++ label ++ ": " ++ card) labeledCards
  let formatted = unlines $ zipWith (\(label, card) i -> show i ++ ". " ++ label ++ ": " ++ card) labeledCards [1..]
      promptText = unlines
        [ "You are a mystical tarot guide.",
          "Respond with information regarding each card and how it fits into the whole picture of the tarot pull. Describe each card in terms of what is depicted on the front of each card according to the Rider Waite tarot deck illustrations. Additionally, provide a culminating synthesis for the reading as well. At the end, include affirmations from the reading.",
          formatted
        ]
  runPrompt promptText

-- Run OpenAI Prompt
runPrompt :: String -> IO ()
runPrompt tarotPrompt = do
  (timestamp, sessionId) <- generateSessionInfo
  putStrLn ""
  putStrWithStyle White $ "📅 Timestamp: " ++ timestamp
  putStrWithStyle White $ "🔖 Session ID: " ++ sessionId
  putStrLn ""
  putStrWithStyle Cyan "✨ Channeling divine wisdom from GPT...\n"
  result <- getTarotInterpretation tarotPrompt
  case result of
    Left err -> putStrWithStyle Red $ "⚠️ Error: " ++ err
    Right interpretation -> do
      putStrWithStyle Magenta "🪄 Interpretation:\n"
      putStrLn interpretation

      let folder = "readings"
          filename = folder ++ "/" ++ sessionId ++ ".md"
          fileContent = unlines
            [ "# 🔮 Arcana Engine Reading",
              "",
              "**📅 Timestamp:** " ++ timestamp,
              "**🔖 Session ID:** `" ++ sessionId ++ "`",
              "",
              "---",
              "",
              "### 🧾 Prompt Sent to GPT",
              "",
              "```text",
              tarotPrompt,
              "```",
              "",
              "---",
              "",
              "### 🪄 Interpretation",
              "",
              interpretation
            ]

      createDirectoryIfMissing True folder
      TIO.writeFile filename (T.pack fileContent)
      putStrWithStyle Blue $ "\n📝 Saved reading to: " ++ filename


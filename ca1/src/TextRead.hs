-- By Mateusz Kolodziejczyk 20084190
module TextRead where

import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Internal.Builder
import Data.Char
import Fmt


extractWords :: Text -> [Text]
extractWords t =  ws
  where
    ws = Prelude.map T.toCaseFold $ Prelude.filter (not . T.null)
         $ Prelude.map cleanWord $ T.words t
    cleanWord = T.dropAround (not . isLetter)

-- allWordsReport :: String -> [Text] -> Text
-- allWordsReport msg words =
--   fmt $ nameF (Data.Text.Internal.Builder.fromString msg)  $ unlinesF words
allWordsReport :: String -> [Text] -> Text
allWordsReport msg words =
    (T.pack msg) <> (T.pack "\n") <> T.unlines ( Prelude.map (\x -> (T.pack " ") <> x) words)

containsCheck :: [a] -> (a -> [a] -> Bool) -> [(Int, a)] -> [(Int, a)] 
containsCheck checkedList func = Prelude.filter (\(_,y) -> func y checkedList)

fixWords :: [Text] -> [(Int, Text)] -> IO [(Int, Text)]
fixWords dict words = do
    let unzipped = unzip words
    let indexes = fst unzipped
    let w = snd unzipped
    newW <- mapM (getWord dict) w
    let y = Prelude.zip indexes newW
    return y

getWord :: [Text] -> Text -> IO Text
getWord dict t = do
    -- Print out the word being changed
    Prelude.putStrLn ""
    print t
    w <- TIO.getLine
    if Prelude.elem w dict 
        then do
            Prelude.putStrLn "Word Found"
            return w
        else do
            Prelude.putStrLn "Not Found in Dictionary"
            getWord dict t

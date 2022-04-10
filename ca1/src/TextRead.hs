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
    (T.pack msg) <> (T.pack "\n") <> T.unlines words

containsCheck :: [a] -> (a -> [a] -> Bool) -> [(Int, a)] -> [(Int, a)] 
containsCheck checkedList func = Prelude.filter (\(_,y) -> func y checkedList)

doEverything :: [Text] -> Text -> IO Text        
doEverything dict word = do
  w <- TIO.getLine
  if Prelude.elem word dict 
    then return w
    else return w
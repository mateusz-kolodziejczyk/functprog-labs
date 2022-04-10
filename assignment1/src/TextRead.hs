module TextRead where

import Data.Text as T
import Data.Text.Internal.Builder
import Data.Char

import Fmt


extractWords :: Text -> [Text]
extractWords t =  ws
  where
    ws = Prelude.map T.toCaseFold $ Prelude.filter (not . T.null)
         $ Prelude.map cleanWord $ T.words t
    cleanWord = T.dropAround (not . isLetter)

allWordsReport :: String -> [Text] -> Text
-- allWordsReport msg words =
--   fmt $ nameF (Data.Text.Internal.Builder.fromString msg)  $ unlinesF words
allWordsReport msg words =
    ""+|msg|+"\n"+|T.unlines words|+""

stringr :: Text -> String
stringr t = do
        print "hello"
        
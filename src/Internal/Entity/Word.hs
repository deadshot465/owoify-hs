{-# LANGUAGE NamedFieldPuns #-}
module Internal.Entity.Word where

import Prelude

import Data.Text.Lazy (strip, Text)
import qualified Data.Text.Lazy (replace)
import Text.RE.TDFA.Text.Lazy ((*=~), anyMatches, matches, RE)
import Text.RE.Replace (replaceAll)
import Data.List (nub)

data InnerWord = InnerWord
  { innerWord :: Text
  , innerReplacedWords :: [Text]
  } deriving (Eq, Show)

testAndGetReplacingWord :: RE -> Text -> Text -> Text
testAndGetReplacingWord searchValue replaceValue str =
  let matchedItems = str *=~ searchValue in
  if anyMatches matchedItems then
    Data.Text.Lazy.replace (head $ matches matchedItems) replaceValue str
  else
    str

containsReplacedWords :: InnerWord -> RE -> Text -> Bool
containsReplacedWords InnerWord { innerReplacedWords } searchValue replaceValue =
  any (\s -> let matchedItems = s *=~ searchValue in
    anyMatches matchedItems && (
    let replacedWord = Data.Text.Lazy.replace (head $ matches matchedItems) replaceValue s in
    replacedWord == s)) innerReplacedWords

buildCollection :: RE -> Text -> [Text]
buildCollection searchValue str = matches $ str *=~ searchValue

buildReplacedWords :: Functor f => Text -> f Text -> f Text
buildReplacedWords replaceValue texts = (\s -> Data.Text.Lazy.replace s replaceValue s) <$> texts

innerReplace :: InnerWord -> RE -> Text -> Bool -> InnerWord
innerReplace word@InnerWord { innerWord, innerReplacedWords } searchValue replaceValue replaceReplacedWords
  | not replaceReplacedWords && containsReplacedWords word searchValue replaceValue = word
  | otherwise = if replacingWord == innerWord then word else InnerWord { innerWord = replacingWord, innerReplacedWords = nub $ innerReplacedWords <> replacedWords }
    where
      matchedItems = innerWord *=~ searchValue
      collection = matches matchedItems
      replacingWord = if head collection == innerWord then strip $ replaceAll replaceValue matchedItems else innerWord
      replacedWords = buildReplacedWords replaceValue collection

innerReplaceWithFuncSingle :: InnerWord -> RE -> (() -> Text) -> Bool -> InnerWord
innerReplaceWithFuncSingle word@InnerWord { innerWord, innerReplacedWords } searchValue f replaceReplacedWords
  | not replaceReplacedWords && containsReplacedWords word searchValue replaceValue = word
  | replacingWord == innerWord = word
  | otherwise = InnerWord { innerWord = replacingWord, innerReplacedWords = nub $ innerReplacedWords <> replacedWords }
  where
      replaceValue = f ()
      replacingWord
        = strip
            $ testAndGetReplacingWord searchValue replaceValue innerWord
      collection = buildCollection searchValue replaceValue
      replacedWords = buildReplacedWords replaceValue collection

innerReplaceWithFuncMultiple :: InnerWord -> RE -> (Text -> Text -> Text) -> Bool -> InnerWord
innerReplaceWithFuncMultiple word@InnerWord { innerWord, innerReplacedWords } searchValue f replaceReplacedWords
  | not $ anyMatches matchedItems = word
  | otherwise =
    if (not replaceReplacedWords && containsReplacedWords word searchValue replaceValue) || (replacingWord == innerWord) then word
    else InnerWord { innerWord = replacingWord, innerReplacedWords = nub $ innerReplacedWords <> replacedWords }
  where
    matchedItems = innerWord *=~ searchValue
    collection = matches matchedItems
    (s1 : s2 : s3 : _) = collection
    replaceValue = f s2 s3
    replacingWord = strip $ Data.Text.Lazy.replace s1 replaceValue innerWord
    replacedWords = buildReplacedWords replaceValue collection
{-# LANGUAGE NamedFieldPuns #-}
module Data.Owoify.Internal.Entity.Word
  ( InnerWord(..)
  , innerReplace
  , innerReplaceWithFuncSingle
  , innerReplaceWithFuncMultiple
  , toText
  )
  where

import Prelude

import Data.Maybe (listToMaybe)
import Data.Text.Lazy (strip, Text)
import qualified Data.Text.Lazy (replace)
import Text.RE.PCRE.Text.Lazy ((*=~), anyMatches, matches, RE)
import Text.RE.Replace (replaceAll)
import Data.List (nub)

-- | Basic type for manipulating strings.
data InnerWord = InnerWord
  { innerWord :: Text
  , innerReplacedWords :: [Text]
  } deriving (Eq, Show)

toText :: InnerWord -> Text
toText InnerWord{ innerWord } = innerWord

testAndGetReplacingWord :: RE -> Text -> Text -> Text
testAndGetReplacingWord searchValue replaceValue str =
  let matchedItems = str *=~ searchValue in
  if anyMatches matchedItems then
    case listToMaybe $ matches matchedItems of
      Nothing -> str
      Just hd -> Data.Text.Lazy.replace hd replaceValue str
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

-- | Match the `word` against `searchValue` and replace matched strings with `replaceValue`.
innerReplace :: InnerWord -> RE -> Text -> Bool -> InnerWord
innerReplace word@InnerWord { innerWord, innerReplacedWords } searchValue replaceValue replaceReplacedWords
  | not replaceReplacedWords && containsReplacedWords word searchValue replaceValue = word
  | otherwise = if replacingWord == innerWord then word else InnerWord { innerWord = replacingWord, innerReplacedWords = nub $ innerReplacedWords <> replacedWords }
    where
      matchedItems = innerWord *=~ searchValue
      collection = matches matchedItems
      replacingWord = case listToMaybe collection of
        Nothing -> innerWord
        Just _ -> strip $ replaceAll replaceValue matchedItems
      replacedWords = buildReplacedWords replaceValue collection

-- | Match the `word` against `searchValue` and replace matched strings with the string resulting from invoking `f`.
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

-- | Match the `word` against `searchValue` and replace matched strings with the string resulting from invoking `f`.
-- 
-- The difference between this and `replaceWithFuncSingle` is that the `f` here takes two `String` arguments.
innerReplaceWithFuncMultiple :: InnerWord -> RE -> (Text -> Text -> Text) -> Bool -> InnerWord
innerReplaceWithFuncMultiple word@InnerWord { innerWord, innerReplacedWords } searchValue f replaceReplacedWords
  | not $ anyMatches matchedItems = word
  | otherwise =
    if (not replaceReplacedWords && containsReplacedWords word searchValue replaceValue) || (replacingWord == innerWord) then word
    else InnerWord { innerWord = replacingWord, innerReplacedWords = nub $ innerReplacedWords <> replacedWords }
  where
    matchedItems = innerWord *=~ searchValue
    collection = matches matchedItems
    (s1 : s2 : s3 : _) = if length collection == 3 then collection else [innerWord, innerWord, innerWord]
    replaceValue = f s2 s3
    replacingWord = strip $ Data.Text.Lazy.replace s1 replaceValue innerWord
    replacedWords = buildReplacedWords replaceValue collection
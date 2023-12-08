module Data.Owoify.Owoify
  ( owoify
  , uwuify
  , uvuify
  , OwoifyLevel(..)
  )
  where

import Prelude hiding (words)

import Data.Functor ((<&>))
import Data.Text.Lazy (Text, intercalate, pack)
import Text.RE.PCRE.Text.Lazy ((*=~), compileRegex, Matches, matches, RE)
import Data.Owoify.Internal.Parser.OwoifyParser (count, OError, OwoifyParser, runParser)
import Data.Owoify.Internal.Data.Presets (owoMappingList, specificWordMappingList, uvuMappingList, uwuMappingList)
import Data.Owoify.Internal.Entity.Word (InnerWord(InnerWord), toText)
import Data.Owoify.Internal.Util.Interleave (interleave)

-- | Levels to denote owoness.
data OwoifyLevel = Owo | Uwu | Uvu

extractWords :: MonadFail f => String -> Text -> f [Text]
extractWords pattern s =
  compileRegex pattern <&> (s *=~) <&> matches

words :: Text -> IO [Text]
words = extractWords "[^\\s]+"

spaces :: Text -> IO [Text]
spaces = extractWords "\\s+"

-- | Owoify source text using the specified level and turn text into nonsensical babyspeaks.
--
-- Examples:
--
-- >>> owoify (Data.Text.Lazy.pack "Hello World!") Owo
-- Hewwo World
owoify :: Text -> OwoifyLevel -> IO Text
owoify source level = do
  w <- words source
  s <- spaces source
  let n = length w
  let parsers = count n $ specificWordMappingList <> (case level of
        Owo -> owoMappingList
        Uwu -> uwuMappingList <> owoMappingList
        Uvu -> uvuMappingList <> uwuMappingList <> owoMappingList) :: OwoifyParser OError [IO InnerWord]
  let result = runParser parsers w
  case result of
    Left e -> do
      error $ show e
      pure $ pack ""
    Right (_, transformedWords) -> do
      wordsList <- sequence transformedWords <&> fmap toText
      let interleaved = reverse $ interleave wordsList s
      pure $ intercalate (pack "") interleaved

-- | Owoify source text using Uwu owoness level and turn text into nonsensical babyspeaks.
--
-- Examples:
--
-- >>> uwuify (Data.Text.Lazy.pack "Hello World!")
-- NOW "Hewwo WowwdUvU"
uwuify :: Text -> IO Text
uwuify source = owoify source Uwu

-- | Owoify source text using Uwu owoness level and turn text into nonsensical babyspeaks.
--
-- Examples:
--
-- >>> uvuify (Data.Text.Lazy.pack "Hello World!")
-- "Hewwowo Wowwd>w<"
uvuify :: Text -> IO Text
uvuify source = owoify source Uvu

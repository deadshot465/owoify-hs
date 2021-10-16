{-# LANGUAGE RankNTypes #-}
module Data.Owoify.Internal.Parser.OwoifyParser
  ( count
  , OError(..)
  , OwoifyError
  , OwoifyParser
  , runParser
  )
  where

import Prelude

import Control.Monad ((>=>), foldM, replicateM)
import Data.Function ((&))
import Data.List (uncons)
import Data.Owoify.Internal.Entity.Word (InnerWord(InnerWord, innerWord, innerReplacedWords)) 
import Data.Text.Lazy (Text)

-- | Represents those types denoting errors when owoifying.
class OwoifyError e where
  -- | Representing that the source collection of strings has been exhausted.
  eof :: e
  -- | Representing general parser error. Currently not used.
  parseError :: Text -> e

-- | A simple type representing errors that occur during owoification.
data OError = EOF | ParseError Text deriving (Show)

instance OwoifyError OError where
  eof = EOF
  parseError = ParseError

type OwoifyResult a = ([Text], a)

type OwoifyFunction e a = OwoifyError e => [Text] -> Either e (OwoifyResult a)

newtype OwoifyParser e a = OwoifyParser (OwoifyFunction e a)

instance Functor (OwoifyParser e) where
  fmap f (OwoifyParser g) = OwoifyParser (fmap (fmap f) . g)

instance Applicative (OwoifyParser e) where
  (<*>) (OwoifyParser f) (OwoifyParser g) = OwoifyParser (f >=> \(s', ab) -> g s' >>= \(s'', a) -> pure (s'', ab a))
  pure x = OwoifyParser (\s -> pure (s, x))

instance Monad (OwoifyParser e) where
  (>>=) (OwoifyParser f) g = OwoifyParser (f >=> \(s', a) -> runParser (g a) s')

-- | Executes (unwraps) the parser inside the monad.
runParser :: OwoifyError e => OwoifyParser e a -> [Text] -> Either e (OwoifyResult a)
runParser (OwoifyParser f) = f

word ::
  (Foldable t, Monad m, OwoifyError e)
  => t (InnerWord -> m InnerWord)
  -> OwoifyParser e (m InnerWord)
word mappings = OwoifyParser (\s ->
  case uncons s of
    Nothing -> Left eof
    Just (head, tail) -> do
      let w = InnerWord { innerWord = head, innerReplacedWords = [] }
      let result = foldM (&) w mappings
      Right (tail, result))

-- | Replicate owoify parser according to the specified length (`n`) and a collection of owoify functions. 
count ::
  (Foldable t, Monad m, OwoifyError e)
  => Int
  -> t (InnerWord -> m InnerWord)
  -> OwoifyParser e [m InnerWord]
count n p | n <= 0 = pure []
          | otherwise = replicateM n $ word p
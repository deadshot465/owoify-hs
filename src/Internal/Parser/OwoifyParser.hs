{-# LANGUAGE RankNTypes #-}
module Internal.Parser.OwoifyParser where

import Prelude

import Control.Monad ( (>=>) )
import Data.Text ( Text )

class OwoifyError e where
  eof :: e

type OwoifyResult a = ([Text], a)

type OwoifyFunction e a = OwoifyError e => [Text] -> Either e (OwoifyResult a)

newtype OwoifyParser e a = OwoifyParser (OwoifyFunction e a)

runParser :: OwoifyError e => OwoifyParser e a -> [Text] -> Either e (OwoifyResult a)
runParser (OwoifyParser f) = f

instance Functor (OwoifyParser e) where
  fmap f (OwoifyParser g) = OwoifyParser (fmap (fmap f) . g)

instance Applicative (OwoifyParser e) where
  (<*>) (OwoifyParser f) (OwoifyParser g) = OwoifyParser (f >=> \(s', ab) -> g s' >>= \(s'', a) -> pure (s'', ab a))
  pure x = OwoifyParser (\s -> pure (s, x))

instance Monad (OwoifyParser e) where
  (>>=) (OwoifyParser f) g = OwoifyParser (f >=> \(s', a) -> runParser (g a) s')
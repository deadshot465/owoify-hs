{-# LANGUAGE BlockArguments #-}
import Prelude

import Data.Text.Lazy (pack, Text)
import Test.HUnit (Test(TestCase, TestLabel, TestList), Assertable (assert), runTestTT)
import Data.Owoify.Owoify (owoify, OwoifyLevel (Owo, Uwu, Uvu))
import Data.Foldable (traverse_)

source :: Text
source = pack "Hello World! This is the string to owo! Kinda cute, isn't it?"

pokemonNamesListPath :: String
pokemonNamesListPath = "assets/pokemons.txt"

warAndPeacePath :: String
warAndPeacePath = "assets/war_and_peace_chapter01-20.txt"

testOwoify :: Test
testOwoify = TestCase (do
  result <- owoify source Owo
  assert $ result /= source)

testOwo :: Test
testOwo = TestCase (do
  result <- owoify source Owo
  assert $ result /= mempty)

testUwu :: Test
testUwu = TestCase (do
  result <- owoify source Uwu
  assert $ result /= mempty)

testUvu :: Test
testUvu = TestCase (do
  result <- owoify source Uvu
  assert $ result /= mempty)

testOwoNotEqualToUwu :: Test
testOwoNotEqualToUwu = TestCase (do
  owoResult <- owoify source Owo
  uwuResult <- owoify source Uwu
  assert $ owoResult /= uwuResult)

testOwoNotEqualToUvu :: Test
testOwoNotEqualToUvu = TestCase (do
  owoResult <- owoify source Owo
  uvuResult <- owoify source Uvu
  assert $ owoResult /= uvuResult)

testUwuNotEqualToUvu :: Test
testUwuNotEqualToUvu = TestCase (do
  uwuResult <- owoify source Uwu
  uvuResult <- owoify source Uvu
  assert $ uwuResult /= uvuResult)

testPokemonNameWithLevel :: Text -> OwoifyLevel -> IO ()
testPokemonNameWithLevel name level = do
  result <- owoify name level
  assert $ result /= pack ""

testPokemonNames :: Test
testPokemonNames = TestCase (do
  pokemonNames <- readFile pokemonNamesListPath
  let pokemonNameList = pack <$> concat (words <$> lines pokemonNames)
  putStrLn "\n---- Test Pokemon Names with Owo"
  traverse_ (`testPokemonNameWithLevel` Owo) pokemonNameList
  putStrLn "\n---- Test Pokemon Names with Uwu"
  traverse_ (`testPokemonNameWithLevel` Uwu) pokemonNameList
  putStrLn "\n---- Test Pokemon Names with Uvu"
  traverse_ (`testPokemonNameWithLevel` Uvu) pokemonNameList)

testLongText :: Test
testLongText = TestCase (do
  longText <- pack <$> readFile warAndPeacePath
  putStrLn "\n---- Test Long Text with Owo"
  owoLongText <- owoify longText Owo
  assert $ owoLongText /= longText
  assert $ owoLongText /= mempty

  putStrLn "\n---- Test Long Text with Uwu"
  uwuLongText <- owoify longText Uwu
  assert $ uwuLongText /= longText
  assert $ uwuLongText /= mempty

  putStrLn "\n---- Test Long Text with Uvu"
  uvuLongText <- owoify longText Uvu
  assert $ uvuLongText /= longText
  assert $ uvuLongText /= mempty)

main :: IO ()
main = do
  let testList = TestList
        [ TestLabel "Test Owoify" testOwoify
        , TestLabel "Test Owo" testOwo
        , TestLabel "Test Uwu" testUwu
        , TestLabel "Test Uvu" testUvu
        , TestLabel "Test Uvu" testUvu
        , TestLabel "Test Owo Not Equal To Uwu" testOwoNotEqualToUwu
        , TestLabel "Test Owo Not Equal To Uvu" testOwoNotEqualToUvu
        , TestLabel "Test Uwu Not Equal To Uvu" testUwuNotEqualToUvu
        , TestLabel "Test Pokemon Names" testPokemonNames
        , TestLabel "Test Long Text" testLongText
        ]
  count <- runTestTT testList
  pure ()

module Internal.Data.Mappings
  ( mapBracketsToStartrails
  , mapConsonantRToConsonantW
  , mapDeadToDed
  , mapEwToUwu
  , mapFiToFwi
  , mapFucToFwuc
  , mapHahaToHeheXd
  , mapHeyToHay
  , mapLeToWal
  , mapLlToWw
  , mapLOrROToWo
  , mapLyToWy
  , mapMeToMwe
  , mapMomToMwom
  , mapNrToNw
  , mapNVowelTToNd
  , mapNVowelToNy
  , mapOldToOwld
  , mapOlToOwl
  , mapOToOwo
  , mapOveToUv
  , mapOverToOwor
  , mapPeriodCommaExclamationSemicolonToKaomojis
  , mapPleToPwe
  , mapPoiToPwoi
  , mapReadToWead
  , mapROrLToW
  , mapRyToWwy
  , mapSpecificConsonantsLeToLetterAndWal
  , mapSpecificConsonantsOToLetterAndWo
  , mapThatToDat
  , mapThToF
  , mapTheToTeh
  , mapTimeToTim
  , mapVerToWer
  , mapVeToWe
  , mapVOrWLeToWal
  , mapVowelOrRExceptOLToWl
  , mapWorseToWose
  , mapYouToU
  )
  where

import Prelude

import Data.Text.Lazy (Text, pack, toUpper)
import Text.RE.TDFA.Text.Lazy (compileRegex, RE)
import System.Random.Stateful (applyAtomicGen, uniformR)
import System.Random (getStdRandom, Random (randomR))
import Internal.Entity.Word (innerReplace, innerReplaceWithFuncMultiple, innerReplaceWithFuncSingle, InnerWord)

compileDefaultOptionRegex :: String -> IO RE
compileDefaultOptionRegex = compileRegex

oToOwo :: IO RE
oToOwo = compileDefaultOptionRegex "o"

ewToUwu :: IO RE
ewToUwu = compileDefaultOptionRegex "ew"

heyToHay :: IO RE
heyToHay = compileDefaultOptionRegex "([Hh])ey"

deadToDedUpper :: IO RE
deadToDedUpper = compileDefaultOptionRegex "Dead"

deadToDedLower :: IO RE
deadToDedLower = compileDefaultOptionRegex "dead"

nVowelTToNd :: IO RE
nVowelTToNd = compileDefaultOptionRegex "n[aeiou]*t"

readToWeadUpper :: IO RE
readToWeadUpper = compileDefaultOptionRegex "Read"

readToWeadLower :: IO RE
readToWeadLower = compileDefaultOptionRegex "read"

bracketsToStartrailsFore :: IO RE
bracketsToStartrailsFore = compileDefaultOptionRegex "[({<]"

bracketsToStartrailsRear :: IO RE
bracketsToStartrailsRear = compileDefaultOptionRegex "[)}>]"

periodCommaExclamationSemicolonToKaomojisFirst :: IO RE
periodCommaExclamationSemicolonToKaomojisFirst = compileDefaultOptionRegex "[.,](?![0-9])"

periodCommaExclamationSemicolonToKaomojisSecond :: IO RE
periodCommaExclamationSemicolonToKaomojisSecond = compileDefaultOptionRegex "[!;]+"

thatToDatUpper :: IO RE
thatToDatUpper = compileDefaultOptionRegex "That"

thatToDatLower :: IO RE
thatToDatLower = compileDefaultOptionRegex "that"

thToFUpper :: IO RE
thToFUpper = compileDefaultOptionRegex "TH(?!E)"

thToFLower :: IO RE
thToFLower = compileDefaultOptionRegex "[Tt]h(?![Ee])"

leToWal :: IO RE
leToWal = compileDefaultOptionRegex "le$"

veToWeUpper :: IO RE
veToWeUpper = compileDefaultOptionRegex "Ve"

veToWeLower :: IO RE
veToWeLower = compileDefaultOptionRegex "ve"

ryToWwy :: IO RE
ryToWwy = compileDefaultOptionRegex "ry"

rOrLToWUpper :: IO RE
rOrLToWUpper = compileDefaultOptionRegex "(?:R|L)"

rOrLToWLower :: IO RE
rOrLToWLower = compileDefaultOptionRegex "(?:r|l)"

llToWw :: IO RE
llToWw = compileDefaultOptionRegex "ll"

vowelOrRExceptOLToWlUpper :: IO RE
vowelOrRExceptOLToWlUpper = compileDefaultOptionRegex "[AEIUR]([lL])$"

vowelOrRExceptOLToWlLower :: IO RE
vowelOrRExceptOLToWlLower = compileDefaultOptionRegex "[aeiur]l$"

oldToOwldUpper :: IO RE
oldToOwldUpper = compileDefaultOptionRegex "OLD"

oldToOwldLower :: IO RE
oldToOwldLower = compileDefaultOptionRegex "([Oo])ld"

olToOwlUpper :: IO RE
olToOwlUpper = compileDefaultOptionRegex "OL"

olToOwlLower :: IO RE
olToOwlLower = compileDefaultOptionRegex "([Oo])l"

lOrROToWoUpper :: IO RE
lOrROToWoUpper = compileDefaultOptionRegex "[LR]([oO])"

lOrROToWoLower :: IO RE
lOrROToWoLower = compileDefaultOptionRegex "[lr]o"

specificConsonantsOToLetterAndWoUpper :: IO RE
specificConsonantsOToLetterAndWoUpper = compileDefaultOptionRegex "([BCDFGHJKMNPQSTXYZ])([oO])"

specificConsonantsOToLetterAndWoLower :: IO RE
specificConsonantsOToLetterAndWoLower = compileDefaultOptionRegex "([bcdfghjkmnpqstxyz])o"

vOrWLeToWal :: IO RE
vOrWLeToWal = compileDefaultOptionRegex "[vw]le"

fiToFwiUpper :: IO RE
fiToFwiUpper = compileDefaultOptionRegex "FI"

fiToFwiLower :: IO RE
fiToFwiLower = compileDefaultOptionRegex "([Ff])i"

verToWer :: IO RE
verToWer = compileDefaultOptionRegex "([Vv])er"

poiToPwoi :: IO RE
poiToPwoi = compileDefaultOptionRegex "([Pp])oi"

specificConsonantsLeToLetterAndWal :: IO RE
specificConsonantsLeToLetterAndWal = compileDefaultOptionRegex "([DdFfGgHhJjPpQqRrSsTtXxYyZz])le$"

consonantRToConsonantW :: IO RE
consonantRToConsonantW = compileDefaultOptionRegex "([BbCcDdFfGgKkPpQqSsTtWwXxZz])r"

lyToWyUpper :: IO RE
lyToWyUpper = compileDefaultOptionRegex "Ly"

lyToWyLower :: IO RE
lyToWyLower = compileDefaultOptionRegex "ly"

pleToPwe :: IO RE
pleToPwe = compileDefaultOptionRegex "([Pp])le"

nrToNwUpper :: IO RE
nrToNwUpper = compileDefaultOptionRegex "NR"

nrToNwLower :: IO RE
nrToNwLower = compileDefaultOptionRegex "nr"

funcToFwuc :: IO RE
funcToFwuc = compileDefaultOptionRegex "([Ff])uc"

momToMwom :: IO RE
momToMwom = compileDefaultOptionRegex "([Mm])om"

meToMwe :: IO RE
meToMwe = compileDefaultOptionRegex "([Mm])e"

nVowelToNyFirst :: IO RE
nVowelToNyFirst = compileDefaultOptionRegex "n([aeiou])"

nVowelToNySecond :: IO RE
nVowelToNySecond = compileDefaultOptionRegex "N([aeiou])"

nVowelToNyThird :: IO RE
nVowelToNyThird = compileDefaultOptionRegex "N([AEIOU])"

oveToUvUpper :: IO RE
oveToUvUpper = compileDefaultOptionRegex "OVE"

oveToUvLower :: IO RE
oveToUvLower = compileDefaultOptionRegex "ove"

hahaToHeheXd :: IO RE
hahaToHeheXd = compileDefaultOptionRegex "\\b(ha|hah|heh|hehe)+\\b"

theToTeh :: IO RE
theToTeh = compileDefaultOptionRegex "\\b([Tt])he\\b"

youToUUpper :: IO RE
youToUUpper = compileDefaultOptionRegex "\\bYou\\b"

youToULower :: IO RE
youToULower = compileDefaultOptionRegex "\\byou\\b"

timeToTim :: IO RE
timeToTim = compileDefaultOptionRegex "\\b([Tt])ime\\b"

overToOwor :: IO RE
overToOwor = compileDefaultOptionRegex "([Oo])ver"

worseToWose :: IO RE
worseToWose = compileDefaultOptionRegex "([Ww])orse"

faces :: [Text]
faces = pack <$>
  [ "(・`ω´・)"
  , ";;w;;"
  , "owo"
  , "UwU"
  , ">w<"
  , "^w^"
  , "(* ^ ω ^)"
  , "(⌒ω⌒)"
  , "ヽ(*・ω・)ﾉ"
  , "(o´∀`o)"
  ,"(o･ω･o)"
  , "＼(＾▽＾)／"
  , "(*^ω^)"
  , "(◕‿◕✿)"
  , "(◕ᴥ◕)"
  , "ʕ•ᴥ•ʔ"
  , "ʕ￫ᴥ￩ʔ"
  , "(*^.^*)"
  , "(｡♥‿♥｡)"
  , "OwO"
  , "uwu"
  , "uvu"
  , "UvU"
  , "(*￣з￣)"
  , "(つ✧ω✧)つ"
  , "(/ =ω=)/"
  , "(╯°□°）╯︵ ┻━┻"
  ,"┬─┬ ノ( ゜-゜ノ)"
  , "¯\\_(ツ)_/¯"
  ]

mapOToOwo :: InnerWord -> IO InnerWord
mapOToOwo word = do
  n <- getStdRandom (randomR (0, 1)) :: IO Int
  let emoji = if n > 0 then pack "owo" else pack "o"
  re <- oToOwo
  pure $ innerReplace word re emoji False

mapEwToUwu :: InnerWord -> IO InnerWord
mapEwToUwu word = ewToUwu >>= \re -> pure $ innerReplace word re (pack "uwu") False

mapHeyToHay :: InnerWord -> IO InnerWord
mapHeyToHay word = heyToHay >>= \re -> pure $ innerReplace word re (pack "$1ay") False

mapDeadToDed :: InnerWord -> IO InnerWord
mapDeadToDed word = do
  w <- deadToDedUpper >>= \re -> pure $ innerReplace word re (pack "Ded") False
  deadToDedLower >>= \re -> pure $ innerReplace w re (pack "ded") False

mapNVowelTToNd :: InnerWord -> IO InnerWord
mapNVowelTToNd word = nVowelTToNd >>= \re -> pure $ innerReplace word re (pack "nd") False

mapReadToWead :: InnerWord -> IO InnerWord
mapReadToWead word = do
  w <- readToWeadUpper >>= \re -> pure $ innerReplace word re (pack "Wead") False
  readToWeadLower >>= \re -> pure $ innerReplace w re (pack "wead") False

mapBracketsToStartrails :: InnerWord -> IO InnerWord
mapBracketsToStartrails word = do
  w <- bracketsToStartrailsFore >>= \re -> pure $ innerReplace word re (pack "｡･:*:･ﾟ★,｡･:*:･ﾟ☆") False
  bracketsToStartrailsRear >>= \re -> pure $ innerReplace w re (pack "☆ﾟ･:*:･｡,★ﾟ･:*:･｡") False

mapPeriodCommaExclamationSemicolonToKaomojis :: InnerWord -> IO InnerWord
mapPeriodCommaExclamationSemicolonToKaomojis word = do
  n <- getStdRandom (randomR (0, length faces - 1)) :: IO Int
  let face = faces !! n
  let w = periodCommaExclamationSemicolonToKaomojisFirst >>= \re -> pure $ innerReplaceWithFuncSingle word re (const face) False
  n' <- getStdRandom (randomR (0, length faces - 1)) :: IO Int
  let face' = faces !! n'
  let re' = periodCommaExclamationSemicolonToKaomojisSecond
  (\r w' ->
    innerReplaceWithFuncSingle w' r (const face') False)
    <$> re'
    <*> w

mapThatToDat :: InnerWord -> IO InnerWord
mapThatToDat word = do
  w <- thatToDatLower >>= \re -> pure $ innerReplace word re (pack "dat") False
  thatToDatUpper >>= \re -> pure $ innerReplace w re (pack "Dat") False

mapThToF :: InnerWord -> IO InnerWord
mapThToF word = do
  w <- thToFLower >>= \re -> pure $ innerReplace word re (pack "f") False
  thToFUpper >>= \re -> pure $ innerReplace w re (pack "F") False

mapLeToWal :: InnerWord -> IO InnerWord
mapLeToWal word = leToWal >>= \re -> pure $ innerReplace word re (pack "wal") False

mapVeToWe :: InnerWord -> IO InnerWord
mapVeToWe word = do
  w <- veToWeLower >>= \re -> pure $ innerReplace word re (pack "we") False
  veToWeUpper >>= \re -> pure $ innerReplace w re (pack "We") False

mapRyToWwy :: InnerWord -> IO InnerWord
mapRyToWwy word = ryToWwy >>= \re -> pure $ innerReplace word re (pack "wwy") False

mapROrLToW :: InnerWord -> IO InnerWord
mapROrLToW word = do
  w <- rOrLToWLower >>= \re -> pure $ innerReplace word re (pack "w") False
  rOrLToWUpper >>= \re -> pure $ innerReplace w re (pack "W") False

mapLlToWw :: InnerWord -> IO InnerWord
mapLlToWw word = llToWw >>= \re -> pure $ innerReplace word re (pack "ww") False

mapVowelOrRExceptOLToWl :: InnerWord -> IO InnerWord
mapVowelOrRExceptOLToWl word = do
  w <- vowelOrRExceptOLToWlLower >>= \re -> pure $ innerReplace word re (pack "wl") False
  vowelOrRExceptOLToWlUpper >>= \re -> pure $ innerReplace w re (pack "W$1") False

mapOldToOwld :: InnerWord -> IO InnerWord
mapOldToOwld word = do
  w <- oldToOwldLower >>= \re -> pure $ innerReplace word re (pack "$1wld") False
  oldToOwldUpper >>= \re -> pure $ innerReplace w re (pack "OWLD") False

mapOlToOwl :: InnerWord -> IO InnerWord
mapOlToOwl word = do
  w <- olToOwlLower >>= \re -> pure $ innerReplace word re (pack "$1wl") False
  olToOwlUpper >>= \re -> pure $ innerReplace w re (pack "OWL") False

mapLOrROToWo :: InnerWord -> IO InnerWord
mapLOrROToWo word = do
  w <- lOrROToWoLower >>= \re -> pure $ innerReplace word re (pack "wo") False
  lOrROToWoUpper >>= \re -> pure $ innerReplace w re (pack "W$1") False

mapSpecificConsonantsOToLetterAndWo :: InnerWord -> IO InnerWord
mapSpecificConsonantsOToLetterAndWo word = do
  w <- specificConsonantsOToLetterAndWoLower >>= \re -> pure $ innerReplace word re (pack "$1wo") False
  specificConsonantsOToLetterAndWoUpper >>= \re ->
    pure $ innerReplaceWithFuncMultiple w re (\c1 c2 -> c1 <> (if toUpper c2 == c2 then pack "W" else pack "w") <> c2) False

mapVOrWLeToWal :: InnerWord -> IO InnerWord
mapVOrWLeToWal word = vOrWLeToWal >>= \re -> pure $ innerReplace word re (pack "wal") False

mapFiToFwi :: InnerWord -> IO InnerWord
mapFiToFwi word = do
  w <- fiToFwiLower >>= \re -> pure $ innerReplace word re (pack "$1wi") False
  fiToFwiUpper >>= \re -> pure $ innerReplace w re (pack "FWI") False

mapVerToWer :: InnerWord -> IO InnerWord
mapVerToWer word = verToWer >>= \re -> pure $ innerReplace word re (pack "wer") False

mapPoiToPwoi :: InnerWord -> IO InnerWord
mapPoiToPwoi word = poiToPwoi >>= \re -> pure $ innerReplace word re (pack "$1woi") False

mapSpecificConsonantsLeToLetterAndWal :: InnerWord -> IO InnerWord
mapSpecificConsonantsLeToLetterAndWal word =
  specificConsonantsLeToLetterAndWal >>= \re -> pure $ innerReplace word re (pack "$1wal") False

mapConsonantRToConsonantW :: InnerWord -> IO InnerWord
mapConsonantRToConsonantW word =
  consonantRToConsonantW >>= \re -> pure $ innerReplace word re (pack "$1w") False

mapLyToWy :: InnerWord -> IO InnerWord
mapLyToWy word = do
  w <- lyToWyLower >>= \re -> pure $ innerReplace word re (pack "wy") False
  lyToWyUpper >>= \re -> pure $ innerReplace w re (pack "Wy") False

mapPleToPwe :: InnerWord -> IO InnerWord
mapPleToPwe word = pleToPwe >>= \re -> pure $ innerReplace word re (pack "$1we") False

mapNrToNw :: InnerWord -> IO InnerWord
mapNrToNw word = do
  w <- nrToNwLower >>= \re -> pure $ innerReplace word re (pack "nw") False
  nrToNwUpper >>= \re -> pure $ innerReplace w re (pack "NW") False

mapFucToFwuc :: InnerWord -> IO InnerWord
mapFucToFwuc word = funcToFwuc >>= \re -> pure $ innerReplace word re (pack "$1wuc") False

mapMomToMwom :: InnerWord -> IO InnerWord
mapMomToMwom word = momToMwom >>= \re -> pure $ innerReplace word re (pack "$1wom") False

mapMeToMwe :: InnerWord -> IO InnerWord
mapMeToMwe word = meToMwe >>= \re -> pure $ innerReplace word re (pack "$1we") False

mapNVowelToNy :: InnerWord -> IO InnerWord
mapNVowelToNy word = do
  w <- nVowelToNyFirst >>= \re -> pure $ innerReplace word re (pack "ny$1") False
  w' <- nVowelToNySecond >>= \re -> pure $ innerReplace w re (pack "Ny$1") False
  nVowelToNyThird >>= \re -> pure $ innerReplace w' re (pack "NY$1") False

mapOveToUv :: InnerWord -> IO InnerWord
mapOveToUv word = do
  w <- oveToUvLower >>= \re -> pure $ innerReplace word re (pack "uv") False
  oveToUvUpper >>= \re -> pure $ innerReplace w re (pack "UV") False

mapHahaToHeheXd :: InnerWord -> IO InnerWord
mapHahaToHeheXd word = hahaToHeheXd >>= \re -> pure $ innerReplace word re (pack "hehe xD") False

mapTheToTeh :: InnerWord -> IO InnerWord
mapTheToTeh word = theToTeh >>= \re -> pure $ innerReplace word re (pack "$1eh") False

mapYouToU :: InnerWord -> IO InnerWord
mapYouToU word = do
  w <- youToUUpper >>= \re -> pure $ innerReplace word re (pack "U") False
  youToULower >>= \re -> pure $ innerReplace w re (pack "u") False

mapTimeToTim :: InnerWord -> IO InnerWord
mapTimeToTim word = timeToTim >>= \re -> pure $ innerReplace word re (pack "$1im") False

mapOverToOwor :: InnerWord -> IO InnerWord
mapOverToOwor word = overToOwor >>= \re -> pure $ innerReplace word re (pack "$1wor") False

mapWorseToWose :: InnerWord -> IO InnerWord
mapWorseToWose word = worseToWose >>= \re -> pure $ innerReplace word re (pack "$1ose") False
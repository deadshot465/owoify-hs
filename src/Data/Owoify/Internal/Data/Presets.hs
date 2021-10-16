module Data.Owoify.Internal.Data.Presets where

import qualified Data.Owoify.Internal.Data.Mappings as Mappings
import Data.Owoify.Internal.Entity.Word ( InnerWord )

specificWordMappingList :: [InnerWord -> IO InnerWord]
specificWordMappingList =
  [ Mappings.mapFucToFwuc
  , Mappings.mapMomToMwom
  , Mappings.mapTimeToTim
  , Mappings.mapMeToMwe
  , Mappings.mapNVowelToNy
  , Mappings.mapOverToOwor
  , Mappings.mapOveToUv
  , Mappings.mapHahaToHeheXd
  , Mappings.mapTheToTeh
  , Mappings.mapYouToU
  , Mappings.mapReadToWead
  , Mappings.mapWorseToWose
  ]

uvuMappingList :: [InnerWord -> IO InnerWord]
uvuMappingList =
  [ Mappings.mapOToOwo
  , Mappings.mapEwToUwu
  , Mappings.mapHeyToHay
  , Mappings.mapDeadToDed
  , Mappings.mapNVowelTToNd
  ]

uwuMappingList :: [InnerWord -> IO InnerWord]
uwuMappingList =
  [ Mappings.mapBracketsToStartrails
  , Mappings.mapPeriodCommaExclamationSemicolonToKaomojis
  , Mappings.mapThatToDat
  , Mappings.mapThToF
  , Mappings.mapLeToWal
  , Mappings.mapVeToWe
  , Mappings.mapRyToWwy
  , Mappings.mapROrLToW
  ]

owoMappingList :: [InnerWord -> IO InnerWord]
owoMappingList =
  [ Mappings.mapLlToWw
  , Mappings.mapVowelOrRExceptOLToWl
  , Mappings.mapOldToOwld
  , Mappings.mapOlToOwl
  , Mappings.mapLOrROToWo
  , Mappings.mapSpecificConsonantsOToLetterAndWo
  , Mappings.mapVOrWLeToWal
  , Mappings.mapFiToFwi
  , Mappings.mapVerToWer
  , Mappings.mapPoiToPwoi
  , Mappings.mapSpecificConsonantsLeToLetterAndWal
  , Mappings.mapConsonantRToConsonantW
  , Mappings.mapLyToWy
  , Mappings.mapPleToPwe
  , Mappings.mapNrToNw
  ]
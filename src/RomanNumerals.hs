module RomanNumerals
    ( stringToRoman
    , integerToRoman
    , integerToRomanString
    ) where

import RomanNumerals.Types
import RomanNumerals.Simple (integerToRomanString)
import qualified RomanNumerals.Parsing as RomanParsing
import Data.List (find)
import Data.Maybe (maybe)


stringToRoman :: String -> Maybe RomVal
stringToRoman str =
  case (RomanParsing.stringToRoman str) of
    Right val -> Just val
    _ -> Nothing


sortedVals :: [RomVal]
sortedVals =
  let
    multiVal :: RomUnit -> [RomVal]
    multiVal u = [RomMul3 u, RomMul2 u, RomVal u]

    valSequence :: (RomVal, RomUnit, RomVal, RomUnit) -> [RomVal]
    valSequence (nine, five, four, one) =
      nine : RomVal five : four : multiVal one
  in
    multiVal RM ++
    valSequence (RomCM, RD, RomCD, RC) ++
    valSequence (RomXC, RL, RomXL, RX) ++
    valSequence (RomIX, RV, RomIV, RI)


makePart :: RomVal -> Maybe RomVal -> RomVal
makePart first second =
  case second of
    Just val -> RomPlus first val
    Nothing -> first


integerToRoman :: Integer -> Maybe RomVal
integerToRoman v = do
  largest <- find ((1 ==) . div v . valToInteger) sortedVals
  return $ maybe largest (RomPlus largest) (integerToRoman (v - valToInteger largest))


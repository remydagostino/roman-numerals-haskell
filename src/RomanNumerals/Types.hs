module RomanNumerals.Types
  ( RomUnit(..)
  , RomVal(..)
  , unitToInteger
  , valToInteger
  ) where

data RomUnit
  = RI
  | RV
  | RX
  | RL
  | RC
  | RD
  | RM

data RomVal
  = RomVal RomUnit
  | RomIV
  | RomIX
  | RomXL
  | RomXC
  | RomCD
  | RomCM
  | RomMul2 RomUnit
  | RomMul3 RomUnit
  | RomPlus RomVal RomVal

instance Show RomUnit where
  show u = case u of
    RI -> "I"
    RV -> "V"
    RX -> "X"
    RL -> "L"
    RC -> "C"
    RD -> "D"
    RM -> "M"

instance Show RomVal where
  show val = case val of
    RomVal u -> show u
    RomIV -> "IV"
    RomIX -> "IX"
    RomXL -> "XL"
    RomXC -> "XC"
    RomCD -> "CD"
    RomCM -> "CM"
    RomMul2 u -> (show u) ++ (show u)
    RomMul3 u -> (show u) ++ (show u) ++ (show u)
    RomPlus v1 v2 -> (show v1) ++ (show v2)

unitToInteger :: RomUnit -> Integer
unitToInteger u = case u of
  RI -> 1
  RV -> 5
  RX -> 10
  RL -> 50
  RC -> 100
  RD -> 500
  RM -> 1000

valToInteger :: RomVal -> Integer
valToInteger val = case val of
  RomVal u -> unitToInteger u
  RomIV -> 4
  RomIX -> 9
  RomXL -> 40
  RomXC -> 90
  RomCD -> 400
  RomCM -> 900
  RomMul2 u -> (unitToInteger u) * 2
  RomMul3 u -> (unitToInteger u) * 3
  RomPlus v1 v2 -> (valToInteger v1) + (valToInteger v2)

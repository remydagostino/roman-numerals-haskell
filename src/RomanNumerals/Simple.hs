module RomanNumerals.Simple (integerToRomanString) where

import Data.List (find)
import Data.Maybe (maybe)

integerToRomanString :: Integer -> Maybe String
integerToRomanString v =
  let
    formTriplet :: Integer -> String -> [(String, Integer)]
    formTriplet num c =
      [ (c ++ c ++ c, num * 3)
      , (c ++ c, num * 2)
      , (c, num)
      ]

    numeralSequence :: Integer -> (String, String, String, String) -> [(String, Integer)]
    numeralSequence baseNum (nine, five, four, one) =
      (nine, baseNum * 9) : (five, baseNum * 5) : (four, baseNum * 4) : formTriplet baseNum one

    sortedVals :: [(String, Integer)]
    sortedVals =
      concat
        [ formTriplet     1000 "M"
        , numeralSequence  100 ("CM", "D", "CD", "C")
        , numeralSequence   10 ("XC", "L", "XL", "X")
        , numeralSequence    1 ("IX", "V", "IV", "I")
        ]
  in do
    (str, num) <- find ((1 ==) . div v . snd) sortedVals
    return $ maybe str (str ++) (integerToRomanString (v - num))

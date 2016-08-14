module RomanNumerals.Simple (integerToRomanString) where

import Data.List (find)
import Data.Maybe (maybe)

integerToRomanString :: Integer -> Maybe String
integerToRomanString v =
  let
    sortedVals :: [(String, Integer)]
    sortedVals =
      [("MMM", 3000)
      ,("MM",  2000)
      ,("M",   1000)
      ,("CM",   900)
      ,("D",    500)
      ,("CD",   400)
      ,("CCC",  300)
      ,("CC",   200)
      ,("C",    100)
      ,("XC",    90)
      ,("L",     50)
      ,("XL",    40)
      ,("XXX",   30)
      ,("XX",    20)
      ,("X",     10)
      ,("IX",     9)
      ,("V",      5)
      ,("IV",     4)
      ,("III",    3)
      ,("II",     2)
      ,("I",      1)
      ]
  in do
    (str, num) <- find ((1 ==) . div v . snd) sortedVals
    return $ maybe str (str ++) (integerToRomanString (v - num))

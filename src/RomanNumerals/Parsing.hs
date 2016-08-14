module RomanNumerals.Parsing
  ( stringToRoman
  ) where

import RomanNumerals.Types
import qualified Text.Parsec.Char as PChar
import qualified Text.Parsec.Combinator as PComb
import qualified Text.Parsec.Prim as PPrim
import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec)
import Data.List (foldl')


stringToRoman :: String -> Either Parsec.ParseError RomVal
stringToRoman =
  Parsec.parse romanParser "roman numerals"


romanParser :: Parsec String st RomVal
romanParser = do
  nums <- Parsec.many singleValParser
  PComb.eof
  sumNumerals nums


sumNumerals :: [RomVal] -> Parsec String st RomVal
sumNumerals nums =
  case findOutOfOrderNumerals nums of
    Just (first, second) ->
      Parsec.parserFail ("Value '" ++ show second ++ "' can not come before '" ++ show first ++ "'")

    Nothing ->
      case nums of
        [] -> Parsec.parserFail ("No roman numerals found")
        [x] -> return x
        (x:xs) -> return $ foldl' RomPlus x xs


findOutOfOrderNumerals :: [RomVal] -> Maybe (RomVal, RomVal)
findOutOfOrderNumerals nums =
  case nums of
    [] -> Nothing
    [x] -> Nothing
    (x:y:xs) ->
      if (valToInteger x) > (valToInteger y)
        then findOutOfOrderNumerals (y:xs)
        else Just (x, y)


singleValParser :: Parsec String st RomVal
singleValParser =
  PComb.choice $ (fmap Parsec.try) $ concat
    [ (handleSpecial 'I' 'V' RomIV)
    , (handleSpecial 'I' 'X' RomIX)
    , (handleSpecial 'X' 'L' RomXL)
    , (handleSpecial 'X' 'C' RomXC)
    , (handleSpecial 'C' 'D' RomCD)
    , (handleSpecial 'C' 'M' RomCM)
    , (handleMult RI)
    , (handleMult RX)
    , (handleMult RC)
    , (handleMult RM)
    , (handleSingle RV)
    , (handleSingle RL)
    , (handleSingle RD)
    ]


handleSpecial :: Char -> Char -> RomVal -> [Parsec String st RomVal]
handleSpecial modifierChar wholeChar val =
  [(PChar.string [modifierChar, wholeChar])
    >> (PComb.notFollowedBy (PChar.oneOf [modifierChar, wholeChar]))
    >> return val
  ]


handleSingle :: RomUnit -> [Parsec String st RomVal]
handleSingle unit =
  [((PChar.string $ show unit) >> return (RomVal unit))]


handleMult :: RomUnit -> [Parsec String st RomVal]
handleMult unit =
  let
    str = PChar.string $ show unit
  in
    [ ((PComb.count 3 str) >> PComb.notFollowedBy str >> return (RomMul3 unit))
    , ((PComb.count 2 str) >> PComb.notFollowedBy str >> return (RomMul2 unit))
    , (str >> PComb.notFollowedBy str >> return (RomVal unit))
    ]

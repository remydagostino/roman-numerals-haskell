module Main where

import RomanNumerals (integerToRoman)

main :: IO ()
main = putStrLn $ (show $ integerToRoman 1)

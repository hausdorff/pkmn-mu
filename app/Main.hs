#!/usr/bin/env stack
-- stack --install-ghc runghc

module Main where

import Prelude hiding (Left, Right)
import Pkmn

testM = parseMap [
  "WX",
  "  ",
  "EW"
  ]

main :: IO ()
-- main = putStrLn $ show $ Edge Entrance Exit $ Star $ Star $ (Lit A <&> Lit B <&> Lit Left) <|> (Lit A <&> Lit B) <&> Lit A <&> Lit A
-- main = putStrLn $ show $ Coord 1 2 Entrance
main = putStrLn $ show testM
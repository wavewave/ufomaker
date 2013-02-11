module Main where

import System.Console.CmdArgs

import HEP.Text.UFO.ProgType
import HEP.Text.UFO.Command

main :: IO () 
main = do 
  putStrLn "ufomaker"
  param <- cmdArgs mode

  commandLineProcess param
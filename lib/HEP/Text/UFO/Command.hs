module HEP.Text.UFO.Command where

import HEP.Text.UFO.ProgType
import HEP.Text.UFO.Job

commandLineProcess :: Ufomaker -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob

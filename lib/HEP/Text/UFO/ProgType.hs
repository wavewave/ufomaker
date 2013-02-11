{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Text.UFO.ProgType where 

import System.Console.CmdArgs

data Ufomaker = Test 
              deriving (Show,Data,Typeable)

test :: Ufomaker
test = Test 

mode = modes [test]


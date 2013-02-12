-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Text.UFO.Type
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Text.UFO.Type where

import Data.Ratio

data Spin = S0 | S12 | S1 | S32 | S2  
  deriving (Show, Eq, Ord)

{- data Statistics = Normal | Opposite

signStatistics Normal = 1 
signStatistics Opposite = -1 
-}


spin2Int :: Spin -> Int
spin2Int S0 =  1
spin2Int S12 = 2 
spin2Int S1 = 3 
spin2Int S32 = 4
spin2Int S2 = 5

isGhost :: Int -> Bool 
isGhost n 
  | n `mod` 2 == 0 = False
  | otherwise = True 


data ColorRep = C1 | C3 | C3bar | C6 | C6bar | C8 
  deriving (Show, Eq, Ord) 

colorRep2Int :: ColorRep -> Int
colorRep2Int C1 = 1
colorRep2Int C3 = 3
colorRep2Int C3bar = -3 
colorRep2Int C6 = 6
colorRep2Int C6bar = -6 
colorRep2Int C8 = 8 


data Param = Zero | Name String | Value Double 

paramShow :: Param -> String 
paramShow Zero = "Param.ZERO"
paramShow (Name str) = str
paramShow (Value v) = show v 


ratioShow :: Rational -> String 
ratioShow x = if denominator x == 1 
              then show (numerator x) 
              else show (numerator x) ++ "/" ++ show (denominator x)

-- data EMCharge = E0 | EP1 | EM1 | EP2 | EM2
--   deriving (Show, Eq, Ord)

data Particle = Particle { ptl_pdg_code :: Int 
                         , ptl_name :: String
                         , ptl_antiname :: String
                         , ptl_spin :: Spin  
                         , ptl_color :: ColorRep 
                         , ptl_mass :: Param 
                         , ptl_width :: Param 
                         , ptl_texname :: String 
                         , ptl_antitexname :: String 
                         , ptl_charge :: Rational 
                         , ptl_ghostNumber :: Int
                         , ptl_leptonNumber :: Rational }  


data ParamType = Real | Complex 
                  
showParamType :: ParamType -> String
showParamType Real = "real"
showParamType Complex = "complex"

data ParamNature = External | Internal 

showParamNature External = "external"
showParamNature Internal = "internal"

data Parameter = Parameter { param_name :: String 
                           , param_nature :: ParamNature 
                           , param_type :: ParamType
                           , param_value :: Double 
                           , param_texname :: String 
                           , param_lhablock :: Maybe String
                           , param_lhacode :: Maybe Int } 
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HEP.Text.UFO.Job where

import Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as L
import Data.Ratio
import System.FilePath 
import Text.Hastache
import Text.Hastache.Context 
-- 
import HEP.Text.UFO.Type
-- 
import Paths_ufomaker 


makeParticleContext :: (String,Particle) -> String -> MuType m 
makeParticleContext (varname,Particle {..}) = context 
  where context "varname" = MuVariable ("ve" :: String)
        context "pdg_code" = MuVariable ptl_pdg_code
        context "name" = MuVariable ptl_name
        context "antiname" = MuVariable ptl_antiname 
        context "spin" = MuVariable (spin2Int ptl_spin 
                                     * if (isGhost ptl_ghostNumber) then -1 else 1 )
        context "color" = MuVariable (colorRep2Int ptl_color)
        context "mass" = MuVariable (paramShow ptl_mass)
        context "width" = MuVariable (paramShow ptl_width)
        context "texname" = MuVariable ptl_texname
        context "antitexname" = MuVariable ptl_antitexname
        context "charge" = MuVariable (ratioShow ptl_charge)
        context "ghostNumber" = MuVariable ptl_ghostNumber
        context "leptonNumber" = MuVariable (ratioShow ptl_leptonNumber)
        context _ = MuNothing


sm_partilces = 
  [ ("ve",Particle 12 "ve" "ve~" S12 C1 Zero Zero "ve" "ve~" 0 0 1)
  , ("vm",Particle 14 "vm" "vm~" S12 C1 Zero Zero "vm" "vm~" 0 0 1)
  , ("vt",Particle 16 "vt" "vt~" S12 C1 Zero Zero "vt" "vt~" 0 0 1)
  , ("u" ,Particle 2  "u"  "u~"  S12 C3 Zero Zero "u"  "u~"  (2%3) 0 0)]


startJob :: IO () 
startJob = do 
  dir <- getDataDir
  let tdir = dir </> "template" 
      particledir = tdir </> "particles"
      complex_particle_ht = particledir </> "complex_particle.ht"
  bstr <- B.readFile complex_particle_ht
  rs <- mapM (hastacheStr defaultConfig bstr . mkStrContext . makeParticleContext)  sm_partilces
  mapM_ L.putStrLn rs 
 where
  context = makeParticleContext 
              ("ve",Particle 12 "ve" "ve~" S12 C1 Zero Zero "ve" "ve~" 0 0 1)


{-  context "pdg_code" = MuVariable (100 :: Int)
  context "varname" = MuVariable ("top" :: String)
  context _ = MuNothin -}

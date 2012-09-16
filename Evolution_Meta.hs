module Evolution_Meta where

import qualified Evolution_01_Freshman
import qualified Evolution_02_Sophomore
import qualified Evolution_03_Junior
import qualified Evolution_04_Junior2
import qualified Evolution_05_Senior
import qualified Evolution_06_Senior2
import qualified Evolution_07_Senior3
import qualified Evolution_08_Memoizing
import qualified Evolution_09_Pointless
import qualified Evolution_10_Iterative
import qualified Evolution_11_IterativeOneLiner
import qualified Evolution_12_Accumulating
import qualified Evolution_13_ContinuationPassing
import qualified Evolution_14_BoyScout
import qualified Evolution_15_Combinatory
import qualified Evolution_16_ListEncoding
import qualified Evolution_17_Interpretive
-- import qualified Evolution_18_Static
-- import qualified Evolution_19_BeginningGraduate
import qualified Evolution_20_Origamist
-- import qualified Evolution_21_CartesianallyInclined
-- import qualified Evolution_22_PHD
-- import qualified Evolution_23_PostDoc
-- import qualified Evolution_24_TenuredProfessor

import Criterion.Main

main = defaultMain [
         bench "Freshman"                $ nf Evolution_01_Freshman.fac                10
       , bench "Sophomore"               $ nf Evolution_02_Sophomore.fac               10
       , bench "Junior"                  $ nf Evolution_03_Junior.fac                  10
       , bench "Junior2"                 $ nf Evolution_04_Junior2.fac                 10
       , bench "Senior"                  $ nf Evolution_05_Senior.fac                  10
       , bench "Senior2"                 $ nf Evolution_06_Senior2.fac                 10
       , bench "Senior3"                 $ nf Evolution_07_Senior3.fac                 10
       , bench "Memoizing"               $ nf Evolution_08_Memoizing.fac               10
       , bench "Pointless"               $ nf Evolution_09_Pointless.fac               10
       , bench "Iterative"               $ nf Evolution_10_Iterative.fac               10
       , bench "IterativeOneLiner"       $ nf Evolution_11_IterativeOneLiner.fac       10
       , bench "Accumulating"            $ nf Evolution_12_Accumulating.fac            10
       , bench "ContinuationPassing"     $ nf Evolution_13_ContinuationPassing.fac     10
       , bench "BoyScout"                $ nf Evolution_14_BoyScout.fac                10
       , bench "Combinatory"             $ nf Evolution_15_Combinatory.fac             10
       , bench "ListEncoding"            $ nf Evolution_16_ListEncoding.fac            10
       , bench "Interpretive"            $ nf Evolution_17_Interpretive.fac            10
       -- , bench "Static"                  $ nf Evolution_18_Static.fac                  10000
       -- , bench "BeginningGraduate"       $ nf Evolution_19_BeginningGraduate.fac       10000
       , bench "Origamist"               $ nf Evolution_20_Origamist.fac               10
       -- , bench "PHD"                     $ nf Evolution_22_PHD.fac                     1000
       -- , bench "PostDoc"                 $ nf Evolution_23_PostDoc.fac                 1000
       -- , bench "TenuredProfessor"        $ nf Evolution_24_TenuredProfessor.fac        1000
       ]

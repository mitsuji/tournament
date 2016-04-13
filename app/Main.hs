{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import for Functional
import Mitsuji.Tournament.Functional ((><),(==>))
import qualified Mitsuji.Tournament.Functional as TF

-- import for Procedural
import Control.Monad.ST (ST,runST,stToIO)
import qualified Mitsuji.Tournament.Procedural as TP
import Debug.Trace (traceM)


newtype S = S String
instance Show S where show (S s) = s



fplayer n = TF.player (S n)

{--
--|[1](たかまさ)
  |--|[2](ゆづき)
  |  |-- [4]ゆづき
  |  |-- [5]はな
  |
  |-- [3]たかまさ

--}
ftest1 =
  let m = TF.toMap $
          (
            fplayer "ゆづき"
            ><
            fplayer "はな"
          )        
          ><
          fplayer "たかまさ" 
--  in TF.graph m
  in TF.graph $ m ==> 4 ==> 3


{--
--|[1](ゆみ)
  |--|[2](ゆづき)
  |  |-- [4]ゆづき
  |  |-- [5]はな
  |
  |--|[3](ゆみ)
  |  |-- [6]たかまさ
  |  |-- [7]ゆみ
  |

--}
ftest2 =
  let m = TF.toMap $
          (
            fplayer "ゆづき"
            ><
            fplayer "はな"
          )
          ><
          (
            fplayer "たかまさ"
            ><
            fplayer "ゆみ"
          )
--  in TF.graph m
  in TF.graph $ m ==> 4 ==> 7 ==> 3


{--
--|[1](じろう)
  |--|[2](じろう)
  |  |-- [4]たろう
  |  |-- [5]じろう
  |
  |--|[3](はな)
  |  |--|[6](はな)
  |  |  |-- [12]ゆづき
  |  |  |-- [13]はな
  |  |
  |  |--|[7](たかまさ)
  |  |  |-- [14]たかまさ
  |  |  |-- [15]ゆみ
  |  |
  |

--}
ftest3 =
  let m = TF.toMap $
          (
            fplayer "たろう"
            ><
            fplayer "じろう"
          )
          ><
          (
            (
              fplayer "ゆづき"
              ><
              fplayer "はな"
            )
            ><
            (
              fplayer "たかまさ"
              ><
              fplayer "ゆみ"
            )
          )
--  in TF.graph m
  in TF.graph $ m ==> 14 ==> 13 ==> 6 ==> 5 ==> 2


fmain = putStr ftest1 >> putStr ftest2 >> putStr ftest3



{--
--|(たかまさ)
  |--|(ゆづき)
  |  |-- ゆづき
  |  |-- はな
  |
  |-- たかまさ

--}
ptest1 = stToIO $ do

  yuzu <- TP.newPlayer $ S "ゆづき"
  hana <- TP.newPlayer $ S "はな"
  taka <- TP.newPlayer $ S "たかまさ"
  
  semiFin <- TP.newBattle yuzu hana
  fin     <- TP.newBattle semiFin taka

  TP.setWinner semiFin 1
  TP.setWinner fin 2
  TP.graph fin >>= traceM
  
  TP.finalists fin >>= traceM . ("finalists: " ++) . show
  TP.winner fin    >>= traceM . ("winner: " ++) . show
  traceM "\n\n" 

  
{--
--|(ゆみ)
  |--|(ゆづき)
  |  |-- ゆづき
  |  |-- はな
  |
  |--|(ゆみ)
  |  |-- たかまさ
  |  |-- ゆみ
  |

--}
ptest2 = stToIO $ do

  yuzu <- TP.newPlayer $ S "ゆづき"
  hana <- TP.newPlayer $ S "はな"
  taka <- TP.newPlayer $ S "たかまさ"
  yumi <- TP.newPlayer $ S "ゆみ"

  semiFin1 <- TP.newBattle yuzu hana
  semiFin2 <- TP.newBattle taka yumi
  fin <- TP.newBattle semiFin1 semiFin2
  
  TP.setWinner semiFin1 1
  TP.setWinner semiFin2 2
  TP.setWinner fin 2
  TP.graph fin >>= traceM

  TP.finalists fin >>= traceM . ("finalists: " ++) . show
  TP.winner fin    >>= traceM . ("winner: " ++) . show
  traceM "\n\n" 


{--
--|(はな)
  |--|(じろう)
  |  |-- たろう
  |  |-- じろう
  |
  |--|(はな)
  |  |--|(はな)
  |  |  |-- ゆづき
  |  |  |-- はな
  |  |
  |  |--|(たかまさ)
  |  |  |-- たかまさ
  |  |  |-- ゆみ
  |  |
  |

--}
ptest3 = stToIO $ do

  taro <- TP.newPlayer $ S "たろう"
  jiro <- TP.newPlayer $ S "じろう"
  yuzu <- TP.newPlayer $ S "ゆづき"
  hana <- TP.newPlayer $ S "はな"
  taka <- TP.newPlayer $ S "たかまさ"
  yumi <- TP.newPlayer $ S "ゆみ"

  semiFin1 <- TP.newBattle taro jiro
  semisemiFin1 <- TP.newBattle yuzu hana
  semisemiFin2 <- TP.newBattle taka yumi
  semiFin2 <- TP.newBattle semisemiFin1 semisemiFin2
  fin <- TP.newBattle semiFin1 semiFin2
  
  TP.setWinner semiFin1 2
  TP.setWinner semisemiFin1 2
  TP.setWinner semisemiFin2 1
  TP.setWinner semiFin2 1
  TP.setWinner fin 2
  TP.graph fin >>= traceM

  TP.finalists fin >>= traceM . ("finalists: " ++) . show
  TP.winner    fin >>= traceM . ("winner: " ++) . show
  traceM "\n\n" 


pmain = ptest1 >> ptest2 >> ptest3



main = fmain >> pmain


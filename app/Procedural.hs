{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Control.Monad.ST (ST,stToIO)
import Data.STRef (STRef,newSTRef,readSTRef,writeSTRef)
import Debug.Trace (traceM)


data Tournament s a = Player a
                    | Battle (STRef s (Tournament s a)) (STRef s (Tournament s a)) (Maybe a)
                      

newPlayer ::  a -> ST s (STRef s (Tournament s a))
newPlayer x = newSTRef $ Player x


newBattle :: STRef s (Tournament s a) -> STRef s (Tournament s a) -> ST s (STRef s (Tournament s a))
newBattle q0 q1 = newSTRef $ Battle q0 q1 Nothing


setWinner :: STRef s (Tournament s a) -> STRef s (Tournament s a) -> ST s ()
setWinner br pr = do
  bp <- (,) <$> readSTRef br <*> readSTRef pr
  case bp of
    (Battle q0 q1 Nothing, Player x) -> writeSTRef br $ Battle q0 q1 (Just x)
    _-> fail $ "setWinner: winner must be a player."


graph :: Show a => STRef s (Tournament s a) -> ST s String
graph tr = T.unpack <$> f tr 0
  where
    f tr d = do
      t <- readSTRef tr
      case t of
        Player x ->
          return $ T.replicate d "  |" +++ "-- " +++ T.pack (show x) +++ "\n"
        Battle qr0 qr1 mx -> do
          t0 <- f qr0 (d+1)
          t1 <- f qr1 (d+1)
          return $
            T.replicate d "  |" +++ "--|" +++ winnerName mx +++ "\n"
            +++ t0 +++ t1 +++ T.replicate d "  |" +++ "\n"
            
    winnerName (Just x) = "(" +++ T.pack (show x) +++ ")"
    winnerName Nothing = ""
    
    (+++) = T.append




newtype S = S String
instance Show S where show (S s) = s

{--
--|(たかまさ)
  |--|(はな)
  |  |-- ゆづき
  |  |-- はな
  |
  |-- たかまさ

--}
test1 = stToIO $ do

  yuzu <- newPlayer $ S "ゆづき"
  hana <- newPlayer $ S "はな"
  taka <- newPlayer $ S "たかまさ"
  
  semiFin <- newBattle yuzu hana
  fin     <- newBattle semiFin taka

  setWinner semiFin hana
  setWinner fin taka
  
  graph fin >>= traceM

  
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
test2 = stToIO $ do

  yuzu <- newPlayer $ S "ゆづき"
  hana <- newPlayer $ S "はな"
  taka <- newPlayer $ S "たかまさ"
  yumi <- newPlayer $ S "ゆみ"

  semiFin1 <- newBattle yuzu hana
  semiFin2 <- newBattle taka yumi
  fin <- newBattle semiFin1 semiFin2
  
  setWinner semiFin1 yuzu
  setWinner semiFin2 yumi
  setWinner fin yumi
  
  graph fin >>= traceM


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
test3 = stToIO $ do

  taro <- newPlayer $ S "たろう"
  jiro <- newPlayer $ S "じろう"
  yuzu <- newPlayer $ S "ゆづき"
  hana <- newPlayer $ S "はな"
  taka <- newPlayer $ S "たかまさ"
  yumi <- newPlayer $ S "ゆみ"

  semiFin1 <- newBattle taro jiro
  semisemiFin1 <- newBattle yuzu hana
  semisemiFin2 <- newBattle taka yumi
  semiFin2 <- newBattle semisemiFin1 semisemiFin2
  fin <- newBattle semiFin1 semiFin2
  
  setWinner semiFin1 jiro
  setWinner semisemiFin1 hana
  setWinner semisemiFin2 taka
  setWinner semiFin2 hana
  setWinner fin hana
  
  graph fin >>= traceM



main = test1 >> test2 >> test3

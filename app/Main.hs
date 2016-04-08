module Main where

import Control.Monad.ST(ST,runST,stToIO)
import Data.STRef(STRef,newSTRef,readSTRef,writeSTRef)


data Tournament s a =
  Battle
  {
    tnmLeft :: STRef s (Tournament s a),
    tnmRight :: STRef s (Tournament s a),
    tnmWinner :: Maybe (STRef s a)
  }
  | Player a


newPlayerST :: a -> ST s (STRef s (Tournament s a))
newPlayerST x = newSTRef $ Player x


newBattleST :: STRef s (Tournament s a) -> STRef s (Tournament s a) -> ST s (STRef s (Tournament s a))
newBattleST l r = newSTRef $ Battle l r Nothing


main = stToIO $ do
  p1 <- newPlayerST "aaa"
  return ()
  

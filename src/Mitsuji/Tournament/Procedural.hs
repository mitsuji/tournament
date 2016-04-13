{-# LANGUAGE OverloadedStrings #-}

module Mitsuji.Tournament.Procedural
       (Tournament,
        newPlayer,
        newBattle,
        winner,
        finalists,
        setWinner,
        graph,
       ) where

import Control.Monad.ST (ST)
import Data.STRef (STRef,newSTRef,readSTRef,writeSTRef)
import qualified Data.Text as T

data Tournament s a =
  Battle
  {
    tnmQualifi1 :: STRef s (Tournament s a),
    tnmQualifi2 :: STRef s (Tournament s a),
    tnmWinner :: Maybe a
  }
  | Player a


newPlayer ::  a -> ST s (STRef s (Tournament s a))
newPlayer x = newSTRef $ Player x


newBattle :: STRef s (Tournament s a) -> STRef s (Tournament s a) -> ST s (STRef s (Tournament s a))
newBattle q1 q2 = newSTRef $ Battle q1 q2 Nothing


winner :: STRef s (Tournament s a) -> ST s (Maybe a)
winner tr = do
  t <- readSTRef tr
  case t of
    Player x -> return $ Just x
    Battle _ _ x -> return x


finalists :: STRef s (Tournament s a) -> ST s (Maybe a, Maybe a)
finalists tr = do
  t <- readSTRef tr
  case t of
    Battle qr1 qr2 _ -> (,) <$> winner qr1 <*> winner qr2
    _ -> fail "finalists: specified data is not battle."


setWinner :: STRef s (Tournament s a) -> Int -> ST s ()
setWinner tr n = do
  t <- readSTRef tr
  case t of
    Battle _ _ Nothing -> do
      w <- winner $ choose t -- get winner of specified qualification
      case w of
        Just (x) -> writeSTRef tr $ t {tnmWinner = Just x}
        Nothing -> fail $ "setWinner: winner of qualification " ++ show n ++ " is not sure."
    _ -> fail "setWinner: winner already exists."
  where
    choose b = case n of
      1 -> tnmQualifi1 b
      2 -> tnmQualifi2 b
      _ -> error "setWinner: choose 1 or 2."


graph :: Show a => STRef s (Tournament s a) -> ST s String
graph tr = T.unpack <$> f tr 0
  where
    f tr d = do
      t <- readSTRef tr
      case t of
        Player x -> return $ T.replicate d "  |" +++ "-- " +++ T.pack (show x) +++ "\n"
        Battle qr1 qr2 w -> do
          t1 <- f qr1 (d+1)
          t2 <- f qr2 (d+1)
          return $
            T.replicate d "  |" +++ "--|" +++ winnerName w +++ "\n"
            +++ t1 +++ t2 +++ T.replicate d "  |" +++ "\n"
            
    winnerName (Just x) = "(" +++ T.pack (show x) +++ ")"
    winnerName Nothing = ""

    (+++) = T.append


{-# LANGUAGE OverloadedStrings #-}

module Mitsuji.Tournament.Functional
       (ID,
        battle,(><),
        player,
        toMap,
        setWinner,(==>),
        graph
       ) where

import qualified Data.Map.Strict as Map
import Data.Bits (shiftR,shiftL)
import qualified Data.Text as T
import Data.Maybe (fromJust)

type ID = Int

data Tournament a = Battle (Maybe ID) (Tournament a) (Tournament a)
                  | Player (Maybe ID) a

battle = Battle Nothing
(><) = battle

player = Player Nothing

fillIDs :: Tournament a -> Tournament a
fillIDs = f 1
  where
    f i (Player _ x) = Player (Just i) x
    f i (Battle _ q0 q1) = Battle (Just i) (f (shiftL i 1) q0) (f ((shiftL i 1)+1) q1)

accumulateIDs :: Tournament a -> Map.Map ID (Maybe a)
accumulateIDs = f
  where
    f (Player (Just i) x ) = Map.insert i (Just x) Map.empty
    f (Battle (Just i) q0 q1) = Map.insert i Nothing $ Map.union (f q0) (f q1)

toMap = accumulateIDs . fillIDs

setWinner :: ID -> Map.Map ID a -> Map.Map ID a
setWinner i m = case Map.lookup i m of
  Just x -> Map.insert (shiftR i 1) x m
  Nothing -> error $ "setWinner: ID " ++ show i ++ " is not found."
(==>) = flip setWinner
  

graph :: Show a => Map.Map ID (Maybe a) -> String
graph m = T.unpack $ f m 1 0
  where
    f m i d =
      let
        x = fromJust $ Map.lookup i m
        (qm0,qm1) = (Map.lookup (shiftL i 1) m, Map.lookup ((shiftL i 1)+1) m)
      in case (qm0,qm1) of
        (Just q0,Just q1) ->
          T.replicate d "  |" +++ "--|" +++ "[" +++ T.pack (show i) +++ "]" +++ winnerName x +++ "\n"
          +++ f m (shiftL i 1) (d+1) +++ f m ((shiftL i 1)+1) (d+1)
          +++ T.replicate d "  |" +++ "\n"
        _ ->
          T.replicate d "  |" +++ "-- " +++ "[" +++ T.pack (show i) +++ "]" +++ playerName x +++ "\n"

    winnerName x = case x of
      Just n  -> "(" +++ T.pack (show n) +++ ")"
      Nothing -> ""
      
    playerName x = T.pack (show (fromJust x))

    (+++) = T.append


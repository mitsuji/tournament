{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Bits (shiftR,shiftL)
import qualified Data.Text as T
import Data.Maybe (fromJust)

type ID = Int

data Tournament a = Player a
                  | Battle (Tournament a) (Tournament a)
(><) = Battle


upperID :: ID -> ID
upperID i = shiftR i 1
             
lowerID :: ID -> Int -> ID
lowerID i d = (shiftL i 1) + d

toMap :: Tournament a -> Map.Map ID (Maybe a)
toMap = f 1
  where
    f i (Player x) = Map.insert i (Just x) Map.empty
    f i (Battle q0 q1) = Map.insert i Nothing $ Map.union (f (lowerID i 0) q0) (f (lowerID i 1) q1)

setWinner :: ID -> Map.Map ID a -> Map.Map ID a
setWinner i m = case Map.lookup i m of
  Just x -> Map.insert (upperID i) x m
  Nothing -> error $ "setWinner: ID " ++ show i ++ " is not found."
(==>) = flip setWinner
  

graph :: Show a => Map.Map ID (Maybe a) -> String
graph m = T.unpack $ f m 1 0
  where
    f m i d =
      let
        mx = fromJust $ Map.lookup i m
        (lowerID0,lowerID1) = (lowerID i 0, lowerID i 1)
        (q0,q1) = (Map.lookup lowerID0 m, Map.lookup lowerID1 m)
                  
      in case (q0,q1) of
        (Nothing, Nothing) ->
          T.replicate d "  |" +++ "-- " +++ "[" +++ T.pack (show i) +++ "]" +++ playerName mx +++ "\n"
        _ ->
          T.replicate d "  |" +++ "--|" +++ "[" +++ T.pack (show i) +++ "]" +++ winnerName mx +++ "\n"
          +++ f m lowerID0 (d+1) +++ f m lowerID1 (d+1)
          +++ T.replicate d "  |" +++ "\n"

    playerName mx = T.pack $ show $ fromJust mx

    winnerName (Just x) = "(" +++ T.pack (show x) +++ ")"
    winnerName Nothing = ""

    (+++) = T.append




newtype S = S String
instance Show S where show (S s) = s

player name = Player (S name)

{--
--|[1](たかまさ)
  |--|[2](はな)
  |  |-- [4]ゆづき
  |  |-- [5]はな
  |
  |-- [3]たかまさ

--}
test1 =
  let m = toMap $
          (
            player "ゆづき"
            ><
            player "はな"
          )        
          ><
          player "たかまさ"
          
  in graph $ m ==> 5 ==> 3


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
test2 =
  let m = toMap $
          (
            player "ゆづき"
            ><
            player "はな"
          )
          ><
          (
            player "たかまさ"
            ><
            player "ゆみ"
          )

  in graph $ m ==> 4 ==> 7 ==> 3


{--
--|[1](はな)
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
test3 =
  let m = toMap $
          (
            player "たろう"
            ><
            player "じろう"
          )
          ><
          (
            (
              player "ゆづき"
              ><
              player "はな"
            )
            ><
            (
              player "たかまさ"
              ><
              player "ゆみ"
            )
          )

  in graph $ m ==> 14 ==> 13 ==> 6 ==> 5 ==> 3


main = putStr test1 >> putStr test2 >> putStr test3

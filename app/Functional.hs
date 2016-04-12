{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as Map
import Data.Bits (shiftR,shiftL)
import qualified Data.Text as T

data Tournament a = Battle (Tournament a) (Tournament a) (Maybe Int) | Player a (Maybe Int)

(***) = \x y -> Battle x y Nothing

setIds :: Tournament a -> Tournament a
setIds = f 1
  where
    f i (Player x _) = Player x (Just i)
    f i (Battle q0 q1 _) = Battle (f (shiftL i 1) q0) (f ((shiftL i 1)+1) q1) (Just i)

getIds :: Tournament a -> Map.Map Int a
getIds = f
  where
    f (Player x (Just i)) = Map.insert i x Map.empty
    f (Battle q0 q1 (Just i)) = Map.union (f q0) (f q1)

setWinner :: Int -> Map.Map Int a -> Map.Map Int a
setWinner i m = case Map.lookup i m of
  Just x -> Map.insert (shiftR i 1) x m
  Nothing -> error $ "setWinner: id " ++ show i ++ " not found."

graph :: Show a => Tournament a -> Map.Map Int a -> String
graph t m = T.unpack $ f t m 0
  where
    f t m d = do
      case t of
        Player x (Just i) ->
          T.replicate d "  |" +++ "-- " +++ "[" +++ T.pack (show i) +++ "]" +++ T.pack (show x) +++ "\n"
        Battle qr1 qr2 (Just i) ->
          T.replicate d "  |" +++ "--|" +++ "[" +++ T.pack (show i) +++ "]" +++ winnerName i +++ "\n"
          +++ f qr1 m (d+1) +++ f qr2 m (d+1)
          +++ T.replicate d "  |" +++ "\n"
        _ -> ""

    winnerName i = case Map.lookup i m of
      Just x  -> "(" +++ T.pack (show x) +++ ")"
      Nothing -> ""

    (+++) = T.append




newtype S = S String
instance Show S where show (S s) = s

player n = Player (S n) Nothing

test1 =
  let t = setIds $
          (
            player "ゆづき"
            ***
            player "はな"
          )        
          ***
          player "たかまさ" 
--  in graph t $ getIds t
  in graph t $ (setWinner 3) $ (setWinner 4) $ getIds t 


test2 =
  let t = setIds $
          (
            player "ゆづき"
            ***
            player "はな"
          )
          ***
          (
            player "たかまさ"
            ***
            player "ゆみ"
          )
--  in graph t $ getIds t
  in graph t $ (setWinner 3) $ (setWinner 7) $ (setWinner 4) $ getIds t 
      

test3 =
  let t = setIds $
          (
            player "たろう"
            ***
            player "じろう"
          )
          ***
          (
            (
              player "ゆづき"
              ***
              player "はな"
            )
            ***
            (
              player "たかまさ"
              ***
              player "ゆみ"
            )
          )
--  in graph t $ getIds t
  in graph t $ (setWinner 2) $ (setWinner 5) $ (setWinner 6) $ (setWinner 13) $ (setWinner 14) $ getIds t


main = putStr test1 >> putStr test2 >> putStr test3


{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.ST (ST,runST,stToIO)
import Data.STRef (STRef,newSTRef,readSTRef,writeSTRef)
import qualified Data.Text as T

import Debug.Trace (traceM)


data Tournament s a =
  Battle
  {
    tnmQualifi1 :: STRef s (Tournament s a),
    tnmQualifi2 :: STRef s (Tournament s a),
    tnmWinner :: Maybe a
  }
  | Player a


newPlayer x = newSTRef $ Player x

newBattle q1 q2 = newSTRef $ Battle q1 q2 Nothing

winner tr = do
  t <- readSTRef tr
  case t of
    Player x -> return $ Just x
    Battle _ _ x -> return x

finalists tr = do
  t <- readSTRef tr
  case t of
    Battle qr1 qr2 _ -> (,) <$> winner qr1 <*> winner qr2
    _ -> fail "finalists: specified data is not battle."

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



newtype S = S String
instance Show S where show (S s) = s

{--
--|(たかまさ)
  |--|(ゆづき)
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
--  graph fin >>= traceM

  setWinner semiFin 1
--  graph fin >>= traceM

  setWinner fin 2
  graph fin >>= traceM

  finalists fin >>= traceM . ("finalists: " ++) . show
  winner fin    >>= traceM . ("winner: " ++) . show
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
test2 = stToIO $ do

  yuzu <- newPlayer $ S "ゆづき"
  hana <- newPlayer $ S "はな"
  taka <- newPlayer $ S "たかまさ"
  yumi <- newPlayer $ S "ゆみ"

  semiFin1 <- newBattle yuzu hana
  semiFin2 <- newBattle taka yumi
  fin <- newBattle semiFin1 semiFin2
--  graph fin >>= traceM
  
  setWinner semiFin1 1
--  graph fin >>= traceM
  
  setWinner semiFin2 2
--  graph fin >>= traceM
  
  setWinner fin 2
  graph fin >>= traceM

  finalists fin >>= traceM . ("finalists: " ++) . show
  winner fin    >>= traceM . ("winner: " ++) . show
  traceM "\n\n" 


{--
--|(じろう)
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
  |--|(じろう)
  |  |-- たろう
  |  |-- じろう
  |

--}
test3 = stToIO $ do

  yuzu <- newPlayer $ S "ゆづき"
  hana <- newPlayer $ S "はな"
  taka <- newPlayer $ S "たかまさ"
  yumi <- newPlayer $ S "ゆみ"
  taro <- newPlayer $ S "たろう"
  jiro <- newPlayer $ S "じろう"

  semisemiFin1 <- newBattle yuzu hana
  semisemiFin2 <- newBattle taka yumi
  semiFin1 <- newBattle semisemiFin1 semisemiFin2
  semiFin2 <- newBattle taro jiro
  fin <- newBattle semiFin1 semiFin2
--  graph fin >>= traceM
  
  setWinner semisemiFin1 2
--  graph fin >>= traceM
  
  setWinner semisemiFin2 1
--  graph fin >>= traceM
  
  setWinner semiFin1 1
--  graph fin >>= traceM
  
  setWinner semiFin2 2
--  graph fin >>= traceM
  
  setWinner fin 2
  graph fin >>= traceM

  finalists fin >>= traceM . ("finalists: " ++) . show
  winner    fin >>= traceM . ("winner: " ++) . show
  traceM "\n\n" 


test1' :: IO ()
test1' = do

  yuzu <- stToIO $ newPlayer $ S "ゆづき"
  hana <- stToIO $ newPlayer $ S "はな"
  taka <- stToIO $ newPlayer $ S "たかまさ"
  
  semiFin <- stToIO $ newBattle yuzu hana
  fin     <- stToIO $ newBattle semiFin taka
--  stToIO (graph fin) >>= putStrLn

  stToIO $ setWinner semiFin 1
--  stToIO (graph fin) >>= putStrLn

  stToIO $ setWinner fin 2
  stToIO (graph fin) >>= putStrLn

  stToIO (finalists fin) >>= putStrLn . ("finalists: " ++) . show
  stToIO (winner fin)    >>= putStrLn . ("winner: " ++) . show
  putStrLn "\n\n" 


main = test1 >> test2 >> test3

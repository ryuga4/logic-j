{-# LANGUAGE TypeOperators #-}

module Lib
    ( someFunc
    ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.HashMap.Strict       as HM
import           Data.List
import Control.Monad.Trans.Class
import Control.Arrow
--import Data.Bifunctor

someFunc :: IO ()
someFunc = putStrLn "someFunc"




data LogicJ = T | F
            | V String
            | Not LogicJ
            | LogicJ :|| LogicJ
            | LogicJ :&& LogicJ
            | LogicJ :== LogicJ
            | LogicJ :-> LogicJ
            deriving (Show, Read, Eq)
{-
instance Show LogicJ where
  show T = "1"
  show F = "0"
  show (V p) = "p"
  show (Not p) = "~" ++ show p
  show (p :|| q) = show p ++ " v " ++ show q
  show (p :&& q) = show p ++ " ^ " ++ show q
  show (p :-> q) = show p ++ " -> " ++ show q
  show (p :== q) = show p ++ " <=>" ++ show q
-}

infixl 2 :||
infixl 2 :&&
infixl 1 :->
infixl 1 :==


---------- TRANSFORMATIONS

-- BASE

buildLogic :: LogicJ -> LogicJ
buildLogic (Not T)   = F
buildLogic (Not F)   = T
buildLogic (T :|| _) = T
buildLogic (_ :|| T) = T
buildLogic (F :&& _) = F
buildLogic (_ :&& F) = F
buildLogic (F :-> _) = T
buildLogic (_ :-> T) = T

-- WITH VARIABLES
buildLogic o@(a :== b) = if buildLogic a == buildLogic b then T else o

buildLogic o @ (V p :|| Not (V q)) | p == q = T
                                   | otherwise = o

buildLogic o @ (V p :&& Not (V q)) | p == q = F
                                   | otherwise = o

buildLogic x = x

---------- EVALUATING

evalLogic :: LogicJ -> HM.HashMap String Bool -> Either String Bool
evalLogic T _ = Right True
evalLogic F _ = Right False
evalLogic (Not p) m = not <$> evalLogic p m
evalLogic (p :|| q) m = liftA2 (||) (evalLogic p m) (evalLogic q m)
evalLogic (p :&& q) m = liftA2 (&&) (evalLogic p m) (evalLogic q m)
evalLogic (p :-> q) m = liftA2 (\p q -> not p || q) (evalLogic p m) (evalLogic q m)
evalLogic (p :== q) m = liftA2 (==) (evalLogic p m) (evalLogic q m)
evalLogic (V p) m = case HM.lookup p m of
  Just x  -> Right x
  Nothing -> Left $ "Can't find value '" ++ p ++ "' in map: \n " ++ show m



---------- FINDING VARIABLES


findVariables :: LogicJ -> [String]
findVariables T         = []
findVariables F         = []
findVariables (Not p)   = findVariables p
findVariables (V p)     = [p]
findVariables (p :|| q) = findVariables p ++ findVariables q
findVariables (p :&& q) = findVariables p ++ findVariables q
findVariables (p :== q) = findVariables p ++ findVariables q
findVariables (p :-> q) = findVariables p ++ findVariables q



---------- COMBINED


checkLogic :: LogicJ -> (LogicJ, [String])
checkLogic l = (buildLogic l, findVariables $ buildLogic l)




--------- INTERFACE


go :: LogicJ -> StateT [(String, Bool)] IO (Either String Bool)
go logic = do
   lift $ print variables
   (flip mapM_) variables $
     \s -> do
         lift $ putStr $ s ++ ": "
         l <- read <$> lift getLine
         modify ((s,l):)
   get >>= return . evalLogic logic' . HM.fromList

  where
    (logic', variables) = second nub $ checkLogic logic


goIO :: IO (Either String Bool)
goIO = do
  logic <- read <$> getLine
  fst <$> runStateT (go logic) []




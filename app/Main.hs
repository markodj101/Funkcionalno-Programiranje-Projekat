{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))

data UserInput = UserInput
  { sourceCode :: String 
  , initialMemory :: String 
  } deriving (Show, Generic)

instance FromJSON UserInput

data ServerResponse = ServerResponse
  { steps :: [ExecutionStep]
  , errorMsg :: Maybe String
  } deriving (Show, Generic)

instance ToJSON ServerResponse

data ExecutionStep = ExecutionStep
  { pc :: Int
  , instruction :: String
  , memorySnapshot :: M.Map String Int
  } deriving (Show, Generic)

instance ToJSON ExecutionStep

type Memory = M.Map String Int

parseMemory :: String -> Memory
parseMemory s = M.fromList $ map parseLine (lines s)
  where
    parseLine l = case words l of
      [k,v] -> (k, read v) 
      _     -> ("err", 0)


runInterpreter :: [String] -> Memory -> [ExecutionStep]
runInterpreter inst mem = go 0 mem 0
  where
    go :: Int -> Memory -> Int -> [ExecutionStep]
    go ip currentMem stepsCount
      | ip >= length inst || stepsCount > 1000 = [] 
      | otherwise =
          let rawLine = inst !! ip
              parts = words rawLine

              currentStep = ExecutionStep ip rawLine currentMem
              
              (nextIp, nextMem) = case parts of
                ("LOAD":r:v:_)    -> (ip + 1, M.insert r (getValue v currentMem) currentMem)
                ("ADD":r:a:b:_)   -> (ip + 1, M.insert r (getValue a currentMem + getValue b currentMem) currentMem)
                ("SUB":r:a:b:_)   -> (ip + 1, M.insert r (getValue a currentMem - getValue b currentMem) currentMem)
                ("MUL":r:a:b:_)   -> (ip + 1, M.insert r (getValue a currentMem * getValue b currentMem) currentMem)
                ("DIV":r:a:b:_)   -> 
                    let valB = getValue b currentMem
                    in if valB == 0 
                       then (length inst, currentMem)
                       else (ip + 1, M.insert r (getValue a currentMem `div` valB) currentMem)
                ("JMP":lbl:_)     -> (findLabel lbl inst, currentMem)
                ("JEQ":a:b:lbl:_) -> 
                    if getValue a currentMem == getValue b currentMem 
                    then (findLabel lbl inst, currentMem)
                    else (ip + 1, currentMem)
                ("JNE":a:b:lbl:_) -> 
                    if getValue a currentMem /= getValue b currentMem 
                    then (findLabel lbl inst, currentMem)
                    else (ip + 1, currentMem)
                ("HALT":_)        -> (length inst, currentMem)
                _                 -> (ip + 1, currentMem)

          in 
             currentStep : go nextIp nextMem (stepsCount + 1)

getValue :: String -> Memory -> Int
getValue s mem = 
    fromMaybe 0 (M.lookup s mem <|> readMaybe s)

findLabel :: String -> [String] -> Int
findLabel lbl allLines = 
  let labelWithColon = lbl ++ ":"
  in fromMaybe (length allLines) (lookupIndex labelWithColon allLines)

lookupIndex :: Eq a => a -> [a] -> Maybe Int
lookupIndex x xs = go 0 xs
  where
    go _ [] = Nothing
    go i (y:ys)
      | x == y    = Just i
      | otherwise = go (i + 1) ys

main :: IO ()
main = scotty 3000 $ do
  middleware $ cors (const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods        = ["POST", "OPTIONS"]
      })

  post "/api/run" $ do
    input <- jsonData :: ActionM UserInput
    
    let instruction = filter (not . null) (lines $ sourceCode input)
   
    let memory = parseMemory (initialMemory input)

    let trace = runInterpreter instruction memory
   
    json $ ServerResponse
     { steps = trace, 
       errorMsg = Nothing 
     }
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO) -- <--- NOVO: Import za random brojeve

-- Ono što primamo od JS-a
data UserInput = UserInput
  { message :: String 
  } deriving (Show, Generic)

instance FromJSON UserInput

-- Ono što šaljemo NAZAD JS-u (sadrži random broj)
data ServerResponse = ServerResponse
  { reply :: String
  , magicNumber :: Int  -- <--- Ovdje će biti random broj
  } deriving (Show, Generic)

instance ToJSON ServerResponse

main :: IO ()
main = scotty 3000 $ do
  -- CORS postavke (kao i prije)
  middleware $ cors (const $ Just simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods        = ["GET", "POST", "OPTIONS"]
      })

  post "/api/log" $ do
    -- 1. Primi poruku
    input <- jsonData :: ActionM UserInput
    
    -- 2. Ispiši u konzolu
    liftIO $ putStrLn $ "Primljeno: " ++ message input

    -- 3. Generiraj random broj (1 do 1000)
    -- Moramo koristiti 'liftIO' jer je randomRIO IO akcija
    rn <- liftIO $ randomRIO (1, 1000) :: ActionM Int

    -- 4. Pošalji odgovor s brojem
    let response = ServerResponse 
          { reply = "Poruka primljena!"
          , magicNumber = rn 
          }
          
    json response
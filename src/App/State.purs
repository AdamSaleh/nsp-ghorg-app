module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Control.Monad (bind, pure)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))
import Data.Either (Either)
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype Results = Results (Array Result)

decodeResult :: Json -> Either String (Array Result)
decodeResult x = decodeJson x 

-- | Decode our Result JSON we receive from the server
instance decodeJsonResults :: DecodeJson Results where
  decodeJson json = do
    obj <- decodeJson json
    items <- obj .? "items" 
    results <- decodeResult items
    pure $ Results $ results

newtype Result = Result
  { path :: String
  , repo :: String }

-- | Decode our Result JSON we receive from the server
instance decodeJsonResult :: DecodeJson Result where
  decodeJson json = do
    obj <- decodeJson json
    path <- obj .? "path"
    repo <- obj .? "repository"
    full_repo <- repo .? "full_name"
    pure $ Result { path : path, repo: full_repo }

newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , results :: Results
  , status :: String
  , username :: String
  , password :: String
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , results: Results []
  , status: "Nothing"
  , username: ""
  , password: ""
  }

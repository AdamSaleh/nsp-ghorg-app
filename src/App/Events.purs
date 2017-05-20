module App.Events where

import App.Routes (Route)
import App.State (State(..), Results)
import Control.Monad (bind, pure)
import Control.Monad.Aff (attempt)
import Data.Argonaut (decodeJson)
import Data.Either (Either(..), either)
import Data.Function (($), (<<<))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)

data Event 
  = PageView Route 
  | Fetch 
  | Done (Either String Results)
  | UsernameChange DOMEvent
  | PasswordChange DOMEvent

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp (Fetch) (State st) = 
  { state: State (st {status = "Fetching"})
  , effects: [ do
      res <- attempt $ affjax $ defaultRequest 
        { url = "https://api.github.com/search/code?q=org:feedhenry+filename:package.json"
        , method = Left GET
        , username = Just st.username
        , password = Just st.password } 
      let decode r = decodeJson r.response :: Either String Results
      let results = either (Left <<< show) decode res
      pure $ Just $ Done results
    ]
  }

-- https://api.nodesecurity.io/advisories

foldp (Done (Right results)) (State state) =
  noEffects $ State (state { results = results, status = "Results" })

foldp (Done (Left err)) (State state) =
  noEffects $ State (state { status = "Error fetching results: " <> show err })

foldp (UsernameChange ev) (State state) =
  noEffects $ State (state { username = targetValue ev })

foldp (PasswordChange ev) (State state) =
  noEffects $ State (state { password = targetValue ev })


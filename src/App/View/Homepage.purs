module App.View.Homepage where

import App.Events (Event(..))
import App.State (Result(..), Results(..), State(..))
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (const, ($))
import Data.Semigroup ((<>))
import Data.Show (show)
import Pux.DOM.Events (onClick, onSubmit, onChange)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (a, button, div, h1, h2, input, li, ol, form)
import Text.Smolder.HTML.Attributes (className, href, name, type', value)
import Text.Smolder.Markup (text, (!), (#!))

results :: Results -> HTML Event
results (Results list) = ol $ for_ list result

result :: Result -> HTML Event
result (Result state) =
  li ! className "result" $ text (state.repo <> state.path)

view :: State -> HTML Event
view (State state) =
  div do
    h1 $ text "List"
    h2 $ text state.status
    div do
      form ! name "signin" $ do
        input ! type' "text" ! value state.username #! onChange UsernameChange
        input ! type' "password" ! value state.password #! onChange PasswordChange
    button #! onClick (const Fetch) $ text "fetch" 
    results state.results


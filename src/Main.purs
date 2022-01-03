module Main where

import Prelude

import Affjax (Response, get, printError)
import Affjax.ResponseFormat (document)

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Argonaut (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Foldable (null, traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (drop)
import Data.Traversable (for_, traverse)

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Web.DOM.Document (Document, toParentNode)
import Web.DOM.Node (Node, textContent)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Location (hash, reload)
import Web.HTML.Window (location, toEventTarget)

import Duration (Duration, showDuration)
import Markup (Markup, attachId_, el, text, (!), (@=))
import Recipe (Instruction(..), Recipe(..), extractRecipe)


main :: Effect Unit
main = launchAff_ $ runExceptT do
  liftEffect $ reloadOnHashChange
  resp <- fetchPage =<< liftEffect getHash
  nodes <- liftEffect $ scrape resp.body
  result <- liftEffect $ extractRecipe <$> traverse textContent nodes
  traverse_ (log <<< printJsonDecodeError) result.left
  attachId_ "contents" $ for_ result.right recipe

reloadOnHashChange :: Effect Unit
reloadOnHashChange = do
  win <- window
  listener <- eventListener $ \_ -> reload =<< location win
  addEventListener (EventType "hashchange") listener false (toEventTarget win)

getHash :: Effect String
getHash = map (drop 1) <<< hash =<< location =<< window

fetchPage :: String -> ExceptT String Aff (Response Document)
fetchPage url = ExceptT $ lmap printError <$> get document (proxy <> url)
  where proxy = "https://proxy.elderephemera.workers.dev?url="

scrape :: Document -> Effect (Array Node)
scrape doc = toArray =<< querySelectorAll query (toParentNode doc)
  where query = QuerySelector "script[type='application/ld+json']"


recipe :: Recipe -> Markup
recipe (Recipe r) = do
  el "h1" $ text r.name
  for_ r.author $ (el "h3" ! "class" @= "author") <<< text
  el "dl" ! "id" @= "times" $ do
    time "Cook" r.cookTime
    time "Prep" r.prepTime
    time "Total" r.totalTime
  for_ r.yield $ (el "span" ! "class" @= "yield") <<< text
  for_ r.description $ (el "div" ! "id" @= "description") <<< text
  unless (null r.ingredients) $ el "div" ! "id" @= "ingredients" $ do
    el "h2" $ text "Ingredients"
    el "ul" $ for_ r.ingredients $ el "li" <<< text
  unless (null r.instructions) $ el "div" ! "id" @= "instructions" $ do
    el "h2" $ text "Instructions"
    el "ol" $ for_ r.instructions $ el "li" <<< instruction

time :: String -> Maybe Duration -> Markup
time label value = for_ value $ \t -> do
  el "dt" $ text $ label <> ":"
  el "dd" $ text $ showDuration t

instruction :: Instruction -> Markup
instruction (InstructionStep { name: Nothing, text: t }) = text t
instruction (InstructionStep { name: Just name, text: t }) = do
  el "span" ! "class" @= "instruction-step-name" $ text name
  el "span" $ text t
instruction (InstructionSection { name, itemListElement: steps }) = do
  el "h4" ! "class" @= "instruction-section-name" $ text name
  el "ol" $ for_ steps instruction

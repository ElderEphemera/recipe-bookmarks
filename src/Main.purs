module Main where

import Prelude

import Affjax (Response, get, printError)
import Affjax.ResponseFormat (string)
import Affjax.ResponseHeader (ResponseHeader(..))

import Control.Monad.Except.Trans (ExceptT(..), runExceptT)

import Data.Argonaut
  (JsonDecodeError, encodeJson, printJsonDecodeError, stringify)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Foldable (any, null, traverse_)
import Data.Maybe (Maybe(..))
import Data.String (drop, length, take, toLower)
import Data.Traversable (for_, traverse)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Web.DOM.Document (toParentNode)
import Web.DOM.DOMParser (parseHTMLFromString, makeDOMParser)
import Web.DOM.Node (textContent)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.HTMLInputElement (fromNode, value)
import Web.HTML.Location (hash, reload, search, setHash, setSearch)
import Web.HTML.Window (document, location, toEventTarget)

import Duration (Duration, showDuration)
import Markup
  (Markup, attachId_, bare, el, el', htmlText, text, (!), (@=), (#=))
import Recipe (Instruction(..), Recipe(..), displayYield, extractRecipe)


main :: Effect Unit
main = addListenerToWindow "load" app

app :: Effect Unit
app = launchAff_ $ (either log pure =<<_) $ runExceptT do
  liftEffect $ addListenerToWindow "hashchange" $ reload =<< location =<< window
  url <- liftEffect getHash
  if url == "" then
    attachId_ "contents" $ landing
  else do
    resp <- fetchPage url
    result <- extractRecipe <$> scrape resp
    case head $ result.right of
      Nothing -> attachId_ "contents" $ error result.left
      Just r -> do
        liftEffect $ setTitleFor r
        param <- liftEffect getSearch
        if (param == "preload")
        then liftEffect $ preloaded r
        else attachId_ "contents" $ recipe r

reloadOnHashChange :: Effect Unit
reloadOnHashChange = do
  win <- window
  listener <- eventListener $ \_ -> reload =<< location win
  addEventListener (EventType "hashchange") listener false (toEventTarget win)

getHash :: Effect String
getHash = map (drop 1) <<< hash =<< location =<< window

getSearch :: Effect String
getSearch = map (drop 1) <<< search =<< location =<< window

fetchPage :: String -> ExceptT String Aff (Response String)
fetchPage url = ExceptT $ lmap printError <$> get string (proxy <> url)
  where proxy = if take 5 url == "data:" then ""
    else "https://proxy.elderephemera.workers.dev?url="

scrape :: Response String -> ExceptT String Aff (Array String)
scrape { headers, body }
  | hasContentType "application/ld+json" headers = pure [body]
  | otherwise = do
    doc <- ExceptT $ liftEffect $ parseHTMLFromString body =<< makeDOMParser
    nodes <- liftEffect $ toArray =<< querySelectorAll query (toParentNode doc)
    liftEffect $ traverse textContent nodes
  where query = QuerySelector "script[type='application/ld+json']"

hasContentType :: String -> Array ResponseHeader -> Boolean
hasContentType ctype = any \(ResponseHeader name val) ->
  toLower name == "content-type" && take (length ctype) val == ctype

setTitleFor :: Recipe -> Effect Unit
setTitleFor (Recipe r) = setTitle ("🔖 " <> r.name) =<< document =<< window

preloaded :: Recipe -> Effect Unit
preloaded r = do
  loc <- location =<< window
  setSearch "" loc
  setHash ("data:application/ld+json," <> stringify (encodeJson r)) loc


landing :: Markup
landing = el "form" do
  el "h1" $ text "Recipe Bookmarks"
  Tuple _ node <- bare $ el' "input" ! "type" @= "text"
    ! "placeholder" @= "Paste recipe URL here"
  for_ (fromNode node) \input ->
    bare $ el "input" ! "type" @= "submit" ! "value" @= "View"
      ! "click" #= \_event -> do
        url <- value input
        setHash url =<< location =<< window

error :: Array JsonDecodeError -> Markup
error errs = el "div" ! "id" @= "error" $ do
  liftEffect $ traverse_ (log <<< printJsonDecodeError) errs
  el "div" $ text $ if null errs
    then "No recipe data found on the provided page"
    else "Could not parse recipe data from the provided page"
  el "button"
    ! "click" #= (\_event -> setHash "" =<< location =<< window)
    $ text "Go Back"

recipe :: Recipe -> Markup
recipe (Recipe r) = do
  el "h1" $ htmlText r.name
  for_ r.author $ (el "h3" ! "class" @= "author") <<< htmlText
  for_ r.id \id -> el "a" ! "id" @= id ! "href" @= id $ text id
  el "dl" ! "id" @= "times" $ do
    time "Cook" r.cookTime
    time "Prep" r.prepTime
    time "Total" r.totalTime
  unless (null r.yield) $ el "div" ! "id" @= "yield" $ do
    for_ r.yield $ el "div" <<< htmlText <<< displayYield
  for_ r.description $ (el "div" ! "id" @= "description") <<< htmlText
  unless (null r.ingredients) $ el "div" ! "id" @= "ingredients" $ do
    el "h2" $ text "Ingredients"
    el "ul" $ for_ r.ingredients $ el "li" <<< htmlText
  unless (null r.instructions) $ el "div" ! "id" @= "instructions" $ do
    el "h2" $ text "Instructions"
    el "ol" $ for_ r.instructions $ el "li" <<< instruction

time :: String -> Maybe Duration -> Markup
time label value = for_ value $ \t -> do
  el "dt" $ text $ label <> ":"
  el "dd" $ text $ showDuration t

instruction :: Instruction -> Markup
instruction (InstructionStep { name: Nothing, text: t }) = htmlText t
instruction (InstructionStep { name: Just name, text: t }) = do
  el "h4" ! "class" @= "instruction-step-name" $ htmlText name
  el "span" $ htmlText t
instruction (InstructionSection { name, itemListElement: steps }) = do
  el "h4" ! "class" @= "instruction-section-name" $ htmlText name
  el "ol" $ for_ steps instruction


addListenerToWindow :: forall y. String -> Effect y -> Effect Unit
addListenerToWindow ev e = do
  win <- window
  listener <- eventListener $ const e
  addEventListener (EventType ev) listener false (toEventTarget win)

module Markup where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (WriterT(..), runWriterT)

import Data.Either (hush)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)

import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.DOM (Document, Node)
import Web.DOM.Document (createElement, createTextNode, documentElement)
import Web.DOM.DOMParser (parseHTMLFromString, makeDOMParser)
import Web.DOM.Element as Elem
import Web.DOM.Node (appendChild, ownerDocument, textContent, toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Text as Text
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


newtype MarkupM a = MarkupM (ReaderT Document (WriterT (Array Node) Effect) a)
type Markup = MarkupM Unit

derive newtype instance functorMarkupM :: Functor MarkupM
derive newtype instance applyMarkupM :: Apply MarkupM
derive newtype instance applicativeMarkupM :: Applicative MarkupM
derive newtype instance bindMarkupM :: Bind MarkupM
derive newtype instance monadMarkupM :: Monad MarkupM

derive newtype instance monadEffectMarkupM :: MonadEffect MarkupM

derive newtype instance semigroupMarkupM :: Semigroup a => Semigroup (MarkupM a)
derive newtype instance monoidadMarkupM :: Monoid a => Monoid (MarkupM a)


runMarkupOn :: forall a. Document -> MarkupM a -> Effect (Tuple a (Array Node))
runMarkupOn doc (MarkupM c) = runWriterT (runReaderT c doc)

onElements
  :: forall a
  .  MarkupM a
  -> (Array Node -> Effect (Array Node))
  -> MarkupM a
onElements c f = MarkupM $ ReaderT \doc -> WriterT do
  Tuple x elements <- runMarkupOn doc c
  newElements <- f elements
  pure (Tuple x newElements)

askDoc :: MarkupM Document
askDoc = MarkupM $ ReaderT pure


el :: forall a. String -> MarkupM a -> MarkupM a
el name c = fst <$> el' name c

el' :: forall a. String -> MarkupM a -> MarkupM (Tuple a Node)
el' name c = do
  doc <- askDoc
  parent <- liftEffect $ Elem.toNode <$> createElement name doc
  x <- onElements c (([parent] <$_) <<< traverse_ (flip appendChild parent))
  pure $ Tuple x parent

text :: String -> Markup
text str = MarkupM $ ReaderT \doc -> WriterT $
  Tuple unit <<< pure <<< Text.toNode <$> createTextNode str doc

htmlText :: String -> Markup
htmlText str = text =<< liftEffect do
  edoc <- parseHTMLFromString str =<< makeDOMParser
  let getText = traverse (textContent <<< Elem.toNode) <=< documentElement
  fromMaybe str <<< join <$> traverse getText (hush edoc)

blank :: Markup
blank = pure unit

bare :: forall a. (MarkupM Unit -> MarkupM a) -> MarkupM a
bare = (_$ blank)


infixl 1 withModifier as !
withModifier
  :: forall a b y
  .  (MarkupM a -> MarkupM b)
  -> (Node -> Effect y)
  -> MarkupM a -> MarkupM b
withModifier f g c =
  onElements (f c) \elements -> elements <$ traverse_ g elements

infix 6 addAttr as @=
addAttr :: String -> String -> Node -> Effect Unit
addAttr name val = traverse_ (Elem.setAttribute name val) <<< Elem.fromNode

infix 6 addListener as #=
addListener :: forall y. String -> (Event -> Effect y) -> Node -> Effect Unit
addListener evType g node = do
  listener <- eventListener g
  addEventListener (EventType evType) listener false (toEventTarget node)


attach :: forall m a. MonadEffect m => Node -> MarkupM a -> m a
attach parent c = liftEffect do
  mdoc <- ownerDocument parent
  case mdoc of
    Just doc -> do
      Tuple x elements <- runMarkupOn doc c
      traverse_ (flip appendChild parent) elements
      pure x
    Nothing -> throw "Cannot attach Markup block to a document"

attachId :: forall m a. MonadEffect m => String -> MarkupM a -> m (Maybe a)
attachId id c = do
  doc <- liftEffect $ document =<< window
  melement <- liftEffect $ getElementById id (toNonElementParentNode doc)
  for melement \element -> attach (Elem.toNode element) c

attachId_ :: forall m a. MonadEffect m => String -> MarkupM a -> m Unit
attachId_ id c = unit <$ attachId id c

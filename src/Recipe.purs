module Recipe where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut
  ( class DecodeJson, class EncodeJson, Json, JsonDecodeError(..)
  , decodeJson, encodeJson, jsonEmptyObject, parseJson
  , (.!=), (.:), (.:?), (~>), (~>?), (:=?), (:=)
  )
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Foreign.Object (Object)

import Duration (Duration)


newtype Recipe = Recipe
  { name :: String
  , author :: Array String
  , id :: Maybe String
  , cookTime :: Maybe Duration
  , prepTime :: Maybe Duration
  , totalTime :: Maybe Duration
  , description :: Maybe String
  , ingredients :: Array String
  , instructions :: Array Instruction
  , yield :: Array Yield
  }

instance decodeRecipe :: DecodeJson Recipe where
  decodeJson = decodeRecipeObj <=< decodeJson

extractRecipe
  :: Array String
  -> { left :: Array JsonDecodeError, right :: Array Recipe }
extractRecipe strs =
  let res0 = partitionMap parseJson strs
      res1 = partitionMap decodeJson res0.right
      res2 = partitionMap decodeJson res0.right
      res3 = foldMap (partitionMap decodeRecipeObj) res2.right
  in res0 { right = [] } <> res1 <> res2 { right = [] } <> res3

decodeRecipeObj :: Object Json -> Either JsonDecodeError Recipe
decodeRecipeObj obj = do
  obj .: "@type" .?= "Recipe"
  name         <- obj .: "name"
  author       <- obj .:* "author"
              <|> (obj .:* "author" >>= traverse (_.: "name"))
  id           <- obj .:? "@id"
  cookTime     <- obj .:? "cookTime"
  prepTime     <- obj .:? "prepTime"
  totalTime    <- obj .:? "totalTime"
  description  <- obj .:? "description"
  ingredients  <- obj .:* "recipeIngredient"
  instructions <- obj .:* "recipeInstructions"
  yield        <- obj .:* "recipeYield"
  pure $ Recipe
    { name, author, id, cookTime, prepTime, totalTime
    , description, ingredients, instructions, yield
    }

instance encodeRecipe :: EncodeJson Recipe where
  encodeJson (Recipe r)
    =   "@type" := "Recipe"
    ~>  "name" := r.name
    ~>  "author" :=* r.author
    ~>* "@id" :=? r.id
    ~>? "cookTime" :=? r.cookTime
    ~>? "prepTime" :=? r.prepTime
    ~>? "totalTime" :=? r.totalTime
    ~>? "description" :=? r.description
    ~>? "recipeIngredient" :=* r.ingredients
    ~>* "recipeInstructions" :=* r.instructions
    ~>* "recipeYield" :=* r.yield
    ~>* jsonEmptyObject


data Instruction
  = InstructionStep
    { name :: Maybe String
    , text :: String
    }
  | InstructionSection
    { name :: String
    , itemListElement :: Array Instruction
    }

instance decodeInstruction :: DecodeJson Instruction where
  decodeJson json
    =   do
          text <- decodeJson json
          pure $ InstructionStep { name: Nothing, text }
    <|> do
          obj <- decodeJson json
          name <- obj .:? "name"
          text <- obj .: "text"
          pure $ InstructionStep { name, text }
    <|> do
          obj <- decodeJson json
          name            <- obj .: "name"
          itemListElement <- obj .:* "itemListElement"
          pure $ InstructionSection { name, itemListElement }

instance encodeInstruction :: EncodeJson Instruction where
  encodeJson (InstructionStep { name: Nothing, text }) = encodeJson text
  encodeJson (InstructionStep { name: Just name, text })
    =  "name" := name
    ~> "text" := text
    ~> jsonEmptyObject
  encodeJson (InstructionSection { name, itemListElement })
    =   "name" := name
    ~>  "itemListElement" :=* itemListElement
    ~>* jsonEmptyObject


data Yield
  = StringYield String
  | IntYield Int

instance decodeYield :: DecodeJson Yield where
  decodeJson json
    =   StringYield <$> decodeJson json
    <|> IntYield <$> decodeJson json

instance encodeYield :: EncodeJson Yield where
  encodeJson (StringYield str) = encodeJson str
  encodeJson (IntYield servings) = encodeJson servings

displayYield :: Yield -> String
displayYield (StringYield str) = str
displayYield (IntYield servings) = show servings <> " servings"


infix 7 pluralField as .:*
pluralField
  :: forall a. DecodeJson a
  => Object Json -> String -> Either JsonDecodeError (Array a)
pluralField obj fld
  =   obj .:? fld .!= []
  <|> (obj .: fld <#> pure)

infix 6 fixedField as .?=
fixedField
  :: forall a. Eq a
  => Either JsonDecodeError a -> a -> Either JsonDecodeError Unit
fixedField field expected = field >>= \actual ->
  if actual == expected then Right unit else Left MissingValue

infix 7 assocPlural as :=*
assocPlural
  :: forall a. EncodeJson a
  => String -> Array a -> Tuple String (Array Json)
assocPlural field = Tuple field <<< map encodeJson

infixr 6 extendPlural as ~>*
extendPlural
  :: forall a. EncodeJson a
  => Tuple String (Array Json) -> a -> Json
extendPlural (Tuple field []) = encodeJson
extendPlural (Tuple field [val]) = ((field := val) ~>_)
extendPlural (Tuple field vals) = ((field := vals) ~>_)

module Recipe where

import Prelude

import Control.Alt ((<|>))

import Data.Argonaut
  ( class DecodeJson, Json, JsonDecodeError(..)
  , decodeJson, parseJson, (.!=), (.:), (.:?)
  )
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

import Foreign.Object (Object)

import Duration (Duration)


newtype Recipe = Recipe
  { name :: String
  , author :: Array String
  , cookTime :: Maybe Duration
  , prepTime :: Maybe Duration
  , totalTime :: Maybe Duration
  , description :: Maybe String
  , ingredients :: Array String
  , instructions :: Array Instruction
  , yield :: Array String
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
  cookTime     <- obj .:? "cookTime"
  prepTime     <- obj .:? "prepTime"
  totalTime    <- obj .:? "totalTime"
  description  <- obj .:? "description"
  ingredients  <- obj .:* "recipeIngredient"
  instructions <- obj .:* "recipeInstructions"
  yield        <- obj .:* "recipeYield"
  pure $ Recipe
    { name, author, cookTime, prepTime, totalTime
    , description, ingredients, instructions, yield
    }


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

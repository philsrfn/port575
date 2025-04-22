-- | This module defines the core data types and initial data for the Haiku Quiz.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module QuizData (
    Question(..)      -- ^ The main data type for a quiz question.
  , initialQuestions  -- ^ A static list of predefined questions.
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON) -- For potential JSON API later

-- | Represents a single quiz question, including the haiku prompt,
--   the expected answer, and the haiku responses for correct/incorrect answers.
data Question = Question
  { haiku     :: [Text]      -- ^ The 3 lines of the question haiku.
  , answer    :: Text        -- ^ The expected correct answer (case-insensitive matching is done in handler).
  , onCorrect :: [Text]      -- ^ The 3 lines of the success haiku.
  , onWrong   :: [Text]      -- ^ The 3 lines of the failure haiku.
  } deriving (Show, Eq, Generic)

-- | Enable automatic derivation of ToJSON/FromJSON instances for the Question type.
-- This allows questions to be easily serialized/deserialized to/from JSON.
instance ToJSON Question
instance FromJSON Question

-- | A predefined list of quiz questions to start with.
--   Questions with haikus that don't fit 5-7-5 have been removed.
initialQuestions :: [Question]
initialQuestions = 
  [ Question
      { haiku =
          [ "Falling apple's path"
          , "What sparked the force of motion?"
          , "Name the thinker's name"
          ]
      , answer = "Newton"
      , onCorrect =
          [ "Your mind sharp as blades"
          , "Truth bends before your keen eyes"
          , "You walk the right path"
          ]
      , onWrong =
          [ "The truth slipped your grip"
          , "A mind vast, but not this time"
          , "Try another path"
          ]
      }
  , Question
      { haiku =
          [ "Code runs, pure and fast"
          , "Lazy evaluation shines"
          , "What tongue speaks this way?"
          ]
      , answer = "Haskell"
      , onCorrect =
          [ "Monads bend to you"
          , "Types align in perfect form"
          , "Function takes its flight"
          ]
      , onWrong =
          [ "Side effects arise"
          , "The syntax seems quite foreign"
          , "Seek the core concept"
          ]
      }
  , Question
      { haiku = [ "Red planet aglow", "Rover searches ancient lakes", "Fourth rock from the sun" ]
      , answer = "Mars"
      , onCorrect = [ "Cosmic neighbor known", "Your knowledge takes to the stars", "Journey's insight found" ]
      , onWrong = [ "Lost in starry night", "Another world, not this one", "Search the void again" ]
      }
  , Question
      { haiku = [ "Hunter waits in grass", "Striped shadow crouched and ready", "Jungle's stealthy king" ]
      , answer = "Tiger"
      , onCorrect = [ "Instinct sharp and true", "The wild heart understands you", "Predator's wisdom" ]
      , onWrong = [ "The trail has gone cold", "A different beast you pictured", "Nature's form misread" ]
      }
  , Question
      { haiku = [ "Seed breaks through the ground", "Leaves drink sunlight joyfully", "Air enriched with life" ]
      , answer = "Plant"
      , onCorrect = [ "Life's cycle unfolds", "The roots of knowledge take hold", "Growth's pattern perceived" ]
      , onWrong = [ "A barren landscape", "The vital spark is missing", "Soil yields no answer" ]
      }
  , Question
      { haiku = [ "Desert sands whisper", "Three giants rise from the dust", "Secrets sealed in stone" ]
      , answer = "Pyramids"
      , onCorrect = [ "Ancient riddle solved", "History's stones speak clearly", "Secrets are unveiled" ]
      , onWrong = [ "Lost in shifting sands", "The monument's name escapes", "Time hides the answer" ]
      }
  ] 
module Errors where

import Data.Text as T

data ErrorReason = ParseError String Text
                 | AmbiguousLoop T.Text
                 deriving(Eq, Show)

data AssembleError = AssembleError {
                      line   :: Int,
                      reason :: ErrorReason
                   }
                   deriving (Eq, Show)
  

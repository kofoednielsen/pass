{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.ServerResponse.Switch
  ( encode
  ) where

import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL

data IntermediateResponse = IntermediateResponse { event :: Text
                                                 , name :: Text
                                                 }
  deriving (Show)

$(deriveJSON defaultOptions ''IntermediateResponse)

encode :: Text -> BL.ByteString
encode playerName =
  let intermediateResponse =
        IntermediateResponse { event = "switch"
                             , name = playerName
                             }
  in A.encode intermediateResponse

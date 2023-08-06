{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.ClientRequest
  ( decode
  ) where

import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Either.Utils (fromRight)

import Types

data IntermediateRequest = IntermediateRequest { name :: Text
                                               , action :: Text
                                               }
  deriving (Show)

$(deriveJSON defaultOptions ''IntermediateRequest)

toPlayerAction :: Text -> PlayerAction
toPlayerAction = \case
  "up" -> GoUp
  "down" -> GoDown
  "left" -> GoLeft
  "right" -> GoRight
  "attack" -> Attack
  "join" -> Join
  "leave" -> Leave
  a -> error ("unexpected action '" ++ T.unpack a ++ "'")

decode :: BL.ByteString -> Request
decode = convert . decodeIntermediate
  where decodeIntermediate :: BL.ByteString -> IntermediateRequest
        decodeIntermediate = fromRight . A.eitherDecode

        convert :: IntermediateRequest -> Request
        convert request = Request { requestName = name request
                                  , requestAction = toPlayerAction (action request)
                                  }

{-# LANGUAGE LambdaCase #-}
module Api.ServerResponse
  ( encode
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Types
import qualified Api.ServerResponse.Switch as Switch
import qualified Api.ServerResponse.NewState as NewState

encode :: Response -> BL.ByteString
encode = \case
  Switch name -> Switch.encode name
  NewState state -> NewState.encode state

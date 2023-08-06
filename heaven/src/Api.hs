module Api
  ( decode
  , decodeString
  , decodeText
  , encode
  , encodeString
  , encodeText
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T

import Types
import qualified Api.ClientRequest as C
import qualified Api.ServerResponse as S

decode :: BL.ByteString -> Request
decode = C.decode

decodeString :: String -> Request
decodeString = decode . BL.pack

decodeText :: Text -> Request
decodeText = decodeString . T.unpack

encode :: Response -> BL.ByteString
encode = S.encode

encodeString :: Response -> String
encodeString = BL.unpack . encode

encodeText :: Response -> Text
encodeText = T.pack . encodeString

module Message where

import qualified Data.Text as T


data Request
  = EchoReq
      { reqEchoData :: T.Text }
  | ReadReq
      { reqReadFileName :: T.Text }
  | WriteReq
      { reqWriteFileName :: T.Text
      , reqWriteContents :: T.Text }

data RepData
  = EchoRepData
      { repEchoData :: T.Text }
  | ReadRepData
      { repReadContents :: T.Text }
  | WriteRepData


data Answer a
  = Answer
  { answError   :: Bool
  , answMessage :: T.Text
  , answData    :: a }


type Reply = Answer RepData

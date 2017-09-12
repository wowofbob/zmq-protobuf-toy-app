module Message where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage

import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import qualified File            as File
import qualified File.ActionType as File
import qualified File.Echo       as File
import qualified File.Request    as FileReq
import qualified File.Reply      as FileRep


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


-- Wrapper for 'messageGet' which returns msg wrapped into 'Maybe'.
messageGet'
  :: (ReflectDescriptor msg, Wire msg)
  => BSL.ByteString
  -> Maybe msg
messageGet' buffer =
  case messageGet buffer of
    Left _         -> Nothing
    Right (msg, _) -> Just msg


parseRequest' :: BSL.ByteString -> Maybe FileReq.Request
parseRequest' = messageGet'

parseReply' :: BSL.ByteString -> Maybe FileRep.Reply
parseReply' = messageGet'


parseRequest :: BSL.ByteString -> Maybe Request
parseRequest buffer =
  do fReq  <- parseRequest' buffer
     case FileReq.type' fReq of
       File.ECHO  ->
         do fEcho <- FileReq.echo fReq
            case File.data' fEcho of
              Utf8 data_ ->
                Just . EchoReq . TE.decodeUtf8 $ BSL.toStrict data_ 
       File.READ  -> Nothing
       File.WRITE -> Nothing

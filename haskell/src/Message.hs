module Message where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage

import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

import qualified File               as File
import qualified File.ActionType    as File
import qualified File.Echo          as File
import qualified File.Reply         as FileRep
import qualified File.Reply.Read    as FileRep
import qualified File.Reply.Write   as FileRep
import qualified File.Request       as FileReq
import qualified File.Request.Read  as FileReqRead
import qualified File.Request.Write as FileReqWrite


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

utf8ToText :: Utf8 -> T.Text
utf8ToText (Utf8 bs) = TE.decodeUtf8 $ BSL.toStrict bs


parseRequest :: BSL.ByteString -> Maybe Request
parseRequest buffer =
  do fReq <- messageGet' buffer
     case FileReq.type' fReq of
       File.ECHO ->
         EchoReq . utf8ToText . File.data' <$> FileReq.echo fReq
       File.READ ->
         ReadReq . utf8ToText . FileReqRead.filename <$> FileReq.read fReq
       File.WRITE ->
          do fRW <- FileReq.write fReq
             let fileName = utf8ToText . FileReqWrite.filename $ fRW
                 contents = utf8ToText . FileReqWrite.contents $ fRW
             Just (WriteReq fileName contents)


parseRepData :: BSL.ByteString -> Maybe RepData
parseRepData buffer =
  do fRep <- messageGet' buffer
     case FileRep.type' fRep of
       File.ECHO ->
         EchoRepData . utf8ToText . File.data' <$> FileRep.echo fRep
       File.READ ->
            -- I didn't noticed that I marked contents
            -- field in read reply as optional :/
         do mContents <- FileRep.contents <$> FileRep.read fRep
            ReadRepData . utf8ToText <$> mContents 
       File.WRITE ->
         do mRW <- FileRep.write fRep
            Just WriteRepData

module Message where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage

import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

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
  { answError   :: Maybe Bool
  , answMessage :: Maybe T.Text
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


parseReply :: BSL.ByteString -> Maybe Reply
parseReply buffer =
  do fRep <- messageGet' buffer
     repData <- case FileRep.type' fRep of
                  File.ECHO ->
                    EchoRepData . utf8ToText . File.data' <$> FileRep.echo fRep
                  File.READ ->
                       -- I didn't noticed that I marked contents
                       -- field in read reply as optional :/
                    do mContents <- FileRep.contents <$> FileRep.read fRep
                       ReadRepData . utf8ToText <$> mContents 
                  File.WRITE ->
                    do _ <- FileRep.write fRep
                       Just WriteRepData
         
         -- Error and message field should be required too I guess.
     let mError   = FileRep.error fRep
         mMessage = utf8ToText <$> FileRep.message fRep
     
     Just (Answer mError mMessage repData)


textToUtf8 :: T.Text -> Utf8
textToUtf8 = Utf8 . BSL.fromStrict . TE.encodeUtf8


reqToMsg :: Request -> FileReq.Request
reqToMsg (EchoReq data_) =
  let type_  = File.ECHO
      echo_  = Just (File.Echo (textToUtf8 data_))
      read_  = Nothing
      write_ = Nothing
    in FileReq.Request type_ echo_ read_ write_
reqToMsg (ReadReq fileName) =
  let type_  = File.READ
      echo_  = Nothing
      read_  = Just (FileReqRead.Read (textToUtf8 fileName))
      write_ = Nothing
    in FileReq.Request type_ echo_ read_ write_
reqToMsg (WriteReq fileName contents) =
  let type_  = File.WRITE
      echo_  = Nothing
      read_  = Nothing
      write_ = Just (FileReqWrite.Write
                      (textToUtf8 fileName)
                      (textToUtf8 contents))
    in FileReq.Request type_ echo_ read_ write_
 
encodeRequest :: Request -> BSL.ByteString
encodeRequest = messagePut . reqToMsg
  
  
repToMsg :: Reply -> FileRep.Reply
repToMsg (Answer mErr mMsg repData) =
  let error_   = mErr
      message_ = textToUtf8 <$> mMsg
      type_    = case repData of
                  EchoRepData  _ -> File.ECHO
                  ReadRepData  _ -> File.READ
                  WriteRepData   -> File.WRITE 
      echo_    = case repData of
                  EchoRepData data_ -> Just . File.Echo $ textToUtf8 data_
                  _ -> Nothing
      read_    = case repData of
                  ReadRepData contents ->
                    Just . FileRep.Read . Just $ textToUtf8 contents
                  _ -> Nothing
      write_   = case repData of
                  WriteRepData -> Just FileRep.Write
                  _ -> Nothing
    in FileRep.Reply type_ error_ message_ echo_ read_ write_ 

encodeReply :: Reply -> BSL.ByteString
encodeReply = messagePut . repToMsg

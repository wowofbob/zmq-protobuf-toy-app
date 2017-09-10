
#include <memory>

#include <message.pb.h>
#include <message.hpp>
#include <message/echo.hpp>
#include <message/read.hpp>
#include <message/write.hpp>


void server()  {}
void client()  {}
void readLog() {}


struct request_handler
  : public echo_handler
  , public read_handler
  , public write_handler {};


int main()
  {
    GOOGLE_PROTOBUF_VERIFY_VERSION;
    
    echo_request  echo_req("hello world!");
    read_request  read_req("temp.txt");
    write_request write_req("foo.txt", "boo");
    
    request_handler h;
    
    std::unique_ptr<echo_reply>  echo_rep0 (echo_handler()(echo_req));
    std::unique_ptr<read_reply>  read_rep0 (read_handler()(read_req));
    std::unique_ptr<write_reply> write_rep0(write_handler()(write_req));
    
    std::unique_ptr<echo_reply>  echo_rep1 (echo_req.dispatch(h));
    std::unique_ptr<read_reply>  read_rep1 (read_req.dispatch(h));
    std::unique_ptr<write_reply> write_rep1(write_req.dispatch(h));
    
  }

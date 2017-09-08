#include <message.pb.h>
#include <message.hpp>

void server()  {}
void client()  {}
void readLog() {}

int main()
  {
    GOOGLE_PROTOBUF_VERIFY_VERSION;
    
    
    msg::req::echo_request er("hello world!");
    
    msg::req::echo_request_handler erh;
   
    er.dispatch(erh);
    erh(er);
    
    msg::req::request_handler req_h;
    //req_h(er);
    er.dispatch(req_h);
    
    msg::req::read_request rr("temp.txt");
    rr.dispatch(req_h);
  }

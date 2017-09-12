
#include <stdlib.h>
#include <stdint.h>

#include <memory>

#include <chrono>
#include <thread>
#include <ratio>


#include <zmq.hpp>

#include <message.pb.h>
#include <message.hpp>
#include <message/echo.hpp>
#include <message/read.hpp>
#include <message/write.hpp>


// Structure with socket.
class with_socket {
  zmq::context_t m_ctx;
  zmq::socket_t  m_skt;
protected:
  zmq::socket_t& get_socket()
    {
      return m_skt;
    }
public:
  with_socket(int type)
    : m_ctx(1)
    , m_skt(m_ctx, type) {}
};

// Socket only for push.
struct push_socket : public with_socket {
  push_socket(char const* addr)
    : with_socket(ZMQ_PUSH)
      {
        get_socket().bind(addr);
      }
  void operator()(std::vector<uint8_t> const& buffer)
    {
      zmq::message_t msg(buffer.size());
      memcpy(msg.data(), buffer.data(), buffer.size());
      get_socket().send(msg);
    }
};

// Socket only for pull.
struct pull_socket : public with_socket {
  pull_socket(char const* addr)
    : with_socket(ZMQ_PULL)
      {
        get_socket().connect(addr);
      }
  void operator()(std::vector<uint8_t>& buffer)
    {
      zmq::message_t reply;
      get_socket().recv(&reply);
      buffer.resize(reply.size());
      memcpy(buffer.data(), reply.data(), buffer.size());
    }
};


// Handler for requests.
struct request_handler
  : public echo_handler
  , public read_handler
  , public write_handler {};

request_handler req_h;
  

// Handler for replies.
struct reply_handler
  : public echo_reply_handler
  , public read_reply_handler
  , public write_reply_handler {};

reply_handler rep_h;
  

// Server part.
void server()
  {
    pull_socket pull("tcp://localhost:5556");
    push_socket push("tcp://*:5555");
    while (true)
      {
        std::vector<uint8_t> in;
        pull(in);
        std::unique_ptr<request_base> req(parse_request(in));
        std::unique_ptr<reply_base>   rep(req->dispatch(req_h));
        
        std::vector<uint8_t> out(rep->encode());
        push(out);
      }
  }
  
  
// Client part.
void client()
  {
    pull_socket pull("tcp://localhost:5555");
    push_socket push("tcp://*:5556");
    while (true)
      {
        echo_request req("hello world!");
        
        std::vector<uint8_t> out(req.encode());
        push(out);
        
        std::vector<uint8_t> in;
        pull(in);
        
        std::unique_ptr<reply_base> rep(parse_reply(in));
        rep->dispatch(rep_h);
        
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
      }
  }


request_base* some_req()
  {
    return new echo_request("hello world!");
  }

int main(int argc, char** argv)
  {
    GOOGLE_PROTOBUF_VERIFY_VERSION;
    
    if (argc != 2)
      {
        return 1;
      }
   
    int type = atoi(argv[1]);
    
    switch (type)
      {
        case 0:
          {
            std::cout << "Server" << std::endl;
            server();
            break;
          }
        case 1:
          {
            std::cout << "Client" << std::endl;
            client();
            break;
          }
      }
    
    
    return 0;
  }

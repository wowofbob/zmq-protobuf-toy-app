
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
class pull_socket : public with_socket {
private:
  zmq::pollitem_t m_poller;
  long            m_timeout;
public:
  pull_socket
    (char const* addr, long timeout = -1)
    : with_socket(ZMQ_PULL)
    , m_poller()
    , m_timeout(timeout)
      {
        get_socket().connect(addr);
        m_poller = { get_socket(), 0, ZMQ_POLLIN, 0};
      }
  bool operator()(std::vector<uint8_t>& buffer)
    {
      zmq::message_t reply;
      zmq::poll(&m_poller, 1, m_timeout);
      if (m_poller.revents & ZMQ_POLLIN)
        {
          get_socket().recv(&reply);
          buffer.resize(reply.size());
          memcpy(buffer.data(), reply.data(), buffer.size());
          return true;
        }
      return false;
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
    uint32_t ctr = 0, ctr_max = 100;
    pull_socket pull("tcp://localhost:5556", 100);
    push_socket push("tcp://*:5555");
    while (true)
      {
        std::cout << "Iteration " << (ctr++) << std::endl;
        
        std::vector<uint8_t> in;
        if (pull(in))
          {
            std::unique_ptr<request_base> req(parse_request(in));
            std::unique_ptr<reply_base>   rep(req->dispatch(req_h));
            
            std::vector<uint8_t> out(rep->encode());
            push(out);
          }
        else
          {
            if (ctr == ctr_max)
              {
                break;
              }
          }
      }
  }
  
  
// Client part.
void client()
  {
    pull_socket pull("tcp://localhost:5555", 100);
    push_socket push("tcp://*:5556");
    while (true)
      {
        std::cout << "CMD: echo, read, write, quit;" << std::endl;
        std::cout << "CMD: ";
        std::string cmd;
        std::getline(std::cin, cmd);
        
        request_base* req = nullptr;
        if (cmd == "quit")
          {
            std::cout << "Exit." << std::endl;
            break;
          }
        else if (cmd == "echo")
          {
            std::cout << "  Data: ";
            std::string data;
            std::getline(std::cin, data);
            req = new echo_request(data);
          }
        else if (cmd == "read")
          {
            std::cout << "  File name: ";
            std::string filename;
            std::getline(std::cin, filename);
            req = new read_request(filename);
          }
        else if (cmd == "write")
          {
            std::cout << "  File name: ";
            std::string filename;
            std::getline(std::cin, filename);
            std::cout << "  Contents:  ";
            std::string contents;
            std::getline(std::cin, contents);
            req = new write_request(filename, contents);
          }
        else
          {
            std::cout << "  Error: command is not supported." << std::endl;
            std::cout << "  Continue."                        << std::endl;
            continue;
          }
        
        if (req != nullptr)
          {            
            std::vector<uint8_t> out(req->encode());
            push(out);
            
            std::vector<uint8_t> in;
            while (!pull(in)) {
              std::this_thread::sleep_for(std::chrono::milliseconds(100));
            }
            
            std::unique_ptr<reply_base> rep(parse_reply(in));
            rep->dispatch(rep_h);
            
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
          }
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

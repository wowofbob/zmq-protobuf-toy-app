
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


class IO {
private:
  zmq::context_t m_ctx;
  zmq::socket_t  m_skt;
public:
  IO(char const* addr, int type)
    : m_ctx(1)
    , m_skt(m_ctx, type)
      {
        if (type == ZMQ_REP)
          {
            m_skt.bind(addr);
          }
        else
          {
            m_skt.connect(addr);
          }
      }
      
  void write(std::vector<uint8_t> const& buffer)
    {
      int i = 0;
      printf("%i\n", (i++));
      
      size_t size = buffer.size();
      zmq::message_t msg(sizeof(size_t) + buffer.size());
      
      printf("%i\n", (i++));
      
      // Copy size.
      memcpy
        ( msg.data()
        , &size
        , sizeof(size_t) );
      
      printf("%i\n", (i++));
      
      // Copy data.
      memcpy
        ( ((uint8_t*)msg.data()) + sizeof(size_t)
        , buffer.data()
        , buffer.size() );
      
      printf("%i\n", (i++));
      m_skt.send(msg);
      
      printf("%i\n", (i++));
    }

  void read(std::vector<uint8_t>& buffer)
    {
      int i = 10;
      printf("%i\n", (i++));
      
      size_t size = 0;
      
      // Get size.
      {
        zmq::message_t header(sizeof(size_t));
        m_skt.recv(&header);
        memcpy(&size, header.data(), sizeof(size_t));
      }
      
      printf("%i\n", (i++));
      
      buffer.resize(size);
      std::cout << "size = " << buffer.size() << std::endl;
      
      // Fill buffer.
      {
        zmq::message_t data(buffer.size());
        m_skt.recv(&data);
        memcpy(buffer.data(), data.data(), buffer.size());
      }
      
      printf("%i\n", (i++));
    }
};


struct request_handler
  : public echo_handler
  , public read_handler
  , public write_handler {};

request_handler req_h;
  

struct reply_handler
  : public echo_reply_handler
  , public read_reply_handler
  , public write_reply_handler {};

reply_handler rep_h;
  

void server()
  {
    IO io("tcp://*:5555", ZMQ_REP);
    while (true)
      {
        std::vector<uint8_t> in;
        io.read(in);
        std::unique_ptr<request_base> req(parse_request(in));
        std::unique_ptr<reply_base>   rep(req->dispatch(req_h));
        std::vector<uint8_t> out(rep->encode());
        io.write(out);
      }
  }
  
void client()
  {
    IO io("tcp://localhost:5555", ZMQ_REQ);
    while (true)
      {
        echo_request req("hello world!");
        
        std::vector<uint8_t> out(req.encode());
        io.write(out);
        
        std::vector<uint8_t> in;
        io.read(in);
        
        std::unique_ptr<reply_base> rep(parse_reply(in));
        rep->dispatch(rep_h);
        
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
      }
  }
  
void readLog() {}


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
          }
        case 1:
          {
            std::cout << "Client" << std::endl;
            client();
          }
      }
    
    
    return 0;
    
    echo_request  echo_req("hello world!");
    read_request  read_req("temp.txt");
    write_request write_req("foo.txt", "boo");
    
    request_handler h;
    reply_handler   rep_h;
    
    
    std::unique_ptr<echo_reply>  echo_rep0 (echo_handler()(echo_req));
    
    
    std::unique_ptr<read_reply>  read_rep0 (read_handler()(read_req));
    std::unique_ptr<write_reply> write_rep0(write_handler()(write_req));
    
    
    std::unique_ptr<echo_reply>  echo_rep1 (echo_req.dispatch(h));
    
    
    std::unique_ptr<read_reply>  read_rep1 (read_req.dispatch(h));
    
    
    std::unique_ptr<write_reply> write_rep1(write_req.dispatch(h));
    
    std::unique_ptr<request_base> echo_req2 (some_req());
    std::unique_ptr<reply_base>   echo_rep2 (echo_req2->dispatch(h));
    
    
    {
      std::vector<uint8_t> buffer = echo_req.encode();
      
      std::unique_ptr<request_base> echo_req3 (parse_request(buffer));
      std::unique_ptr<reply_base>   echo_rep3 (echo_req3->dispatch(h));
      echo_rep3->dispatch(rep_h);
    }
    
    {
      std::vector<uint8_t> buffer = read_req.encode();

      std::unique_ptr<request_base> read_req3 (parse_request(buffer));
      std::unique_ptr<reply_base>   read_rep3 (read_req3->dispatch(h));
      read_rep3->dispatch(rep_h);
    }

    {
      std::vector<uint8_t> buffer = write_req.encode();

      std::unique_ptr<request_base> write_req3 (parse_request(buffer));
      std::unique_ptr<reply_base>   write_rep3 (write_req3->dispatch(h));
      write_rep3->dispatch(rep_h);
    }
    
    echo_reply echo_rep4(false, "OK", "hello world!");
    echo_rep4.dispatch(rep_h);
    
    std::unique_ptr<reply_base>
      echo_rep5(new echo_reply(false, "OK", "hello again!"));
    
    echo_rep5->dispatch(rep_h);
    
    std::unique_ptr<reply_base>
      echo_rep6(echo_req.dispatch(h));
    echo_rep6->dispatch(rep_h);

    
    {
      std::vector<uint8_t> buffer = echo_req.encode();

      std::unique_ptr<request_base> echo_req7 (parse_request(buffer));
      std::unique_ptr<reply_base>   echo_rep7 (echo_req7->dispatch(h));
      echo_rep7->dispatch(rep_h);
    }
    
  }

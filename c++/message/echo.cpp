#include "echo.hpp"

#include <message.pb.h>

#include <iostream>


// Request.

echo_request::
  echo_request(std::string const& data)
    : m_data(data) {}

std::string const& echo_request::data() const
  {
    return m_data;
  }


// Reply.

echo_reply::
  echo_reply
    ( bool error
    , std::string const& message
    , std::string const& data    )
      : reply(error, message)
      , m_data(data) {} 

std::string const& echo_reply::data() const
  {
    return m_data;
  }


// Handler.
echo_reply* echo_handler::
  operator()(echo_request const& req)
    {
      std::cout << "ECHO: " << req.data() << std::endl;
      std::string msg("OK");
      echo_reply* rep = new echo_reply(false, msg, req.data());
      return rep;
    }
    
void echo_reply_handler::
  operator()(echo_reply const& rep)
    {
      std::cout
        << "ECHO\n"
        << "  error:    " << rep.error()   << std::endl
        << "  message:  " << rep.message() << std::endl
        << "  data:     " << rep.data()    << std::endl << std::endl;
    }


// Encode.

std::vector<uint8_t> echo_reply::encode() const
  {
    ::file::Reply rep;
    
    rep.set_type(::file::ECHO);
    rep.set_error(error());
    rep.set_message(message());
    
    ::file::Echo* echo = new ::file::Echo();
    echo->set_data(data());
    
    rep.set_allocated_echo(echo);
    
    std::vector<uint8_t> buffer(rep.ByteSizeLong());
    rep.SerializeToArray(buffer.data(), buffer.size());
    
    return buffer;
  }

std::vector<uint8_t> echo_request::encode() const
  {
    ::file::Request req;
    
    req.set_type(::file::READ);
    
    ::file::Echo* echo = new ::file::Echo();
    echo->set_data(data());
    
    req.set_allocated_echo(echo);
    
    std::vector<uint8_t> buffer(req.ByteSizeLong());
    req.SerializeToArray(buffer.data(), buffer.size());
    
    return buffer;
    
  }

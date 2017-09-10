#include "echo.hpp"

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
      return new echo_reply(false, "OK", req.data());
    }

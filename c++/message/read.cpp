#include "read.hpp"

#include <iostream>


// Request.

read_request::
  read_request(std::string const& filename)
    : m_filename(filename) {}
    
std::string const& read_request::filename() const
  {
    return m_filename;
  }


// Reply.

read_reply::
  read_reply
    ( bool error
    , std::string const& message
    , std::string const& contents )
      : reply(error, message)
      , m_contents(contents) {}

std::string const& read_reply::contents() const
  {
    return m_contents;
  }


// Handler.
read_reply* read_handler::
  operator()(read_request const& req)
    {
      std::cout << "READ: " << req.filename() << std::endl;
      return new read_reply(false, "OK", "...");
    }

#include "write.hpp"

#include <iostream>


// Request.

write_request::
  write_request
    ( std::string const& filename
    , std::string const& contents )
      : m_filename(filename)
      , m_contents(contents) {}
    
std::string const& write_request::filename() const
  {
    return m_filename;
  }
  
std::string const& write_request::contents() const
  {
    return m_contents;
  }

write_reply* write_request::dispatch(handler_base& h)
  {
    dyn_dispatch(h, *this);
  }


// Reply.

write_reply::
  write_reply
    ( bool error
    , std::string const& message )
      : reply(error, message) {}


// Handler.
write_reply* write_handler::
  operator()(write_request const& req)
    {
      std::cout << "WRITE: " << req.filename() << ' ' <<  req.contents() << std::endl;
      return new write_reply(false, "OK");
    }

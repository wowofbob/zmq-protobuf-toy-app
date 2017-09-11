#include "write.hpp"

#include <message.pb.h>

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

void write_reply_handler::
  operator()(write_reply const& rep)
    {
      std::cout
        << "WRITE\n"
        << "  error:    " << rep.error()    << std::endl
        << "  message:  " << rep.message()  << std::endl << std::endl;
    }


// Encode.

std::vector<uint8_t> write_reply::encode() const
  {
    ::file::Reply rep;
    
    rep.set_type(::file::WRITE);
    rep.set_error(error());
    rep.set_message(message());
    
    ::file::Reply_Write* write = new ::file::Reply_Write();
    
    rep.set_allocated_write(write);
    
    std::vector<uint8_t> buffer(rep.ByteSizeLong());
    rep.SerializeToArray(buffer.data(), buffer.size());
    
    return buffer;
  }
  
std::vector<uint8_t> write_request::encode() const
  {
    ::file::Request req;
    
    req.set_type(::file::WRITE);
    
    ::file::Request_Write* write = new ::file::Request_Write();
    
    write->set_filename(filename());
    write->set_contents(contents());
    
    req.set_allocated_write(write);
    
    std::vector<uint8_t> buffer(req.ByteSizeLong());
    req.SerializeToArray(buffer.data(), buffer.size());
    
    return buffer;
  }

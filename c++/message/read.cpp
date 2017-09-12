#include "read.hpp"

#include <message.pb.h>

#include <fstream>
#include <iostream>
#include <sstream>


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
      std::cout
        << "Received READ\n"
        << "  filename: " << req.filename() << std::endl << std::endl;
      
      read_reply* rep = nullptr;
      
      std::ifstream in(req.filename());
      if (in.is_open())
        {
          std::stringstream sstr;
          while (in >> sstr.rdbuf());
          rep = new read_reply(false, "OK", sstr.str());
        }
      else
        {
          rep = new read_reply(true, "read file error", "");
        }
      
      return rep;
    }
    
void read_reply_handler::
  operator()(read_reply const& rep)
    {
      std::cout
        << "READ\n"
        << "  error:    " << rep.error()    << std::endl
        << "  message:  " << rep.message()  << std::endl
        << "  contents: " << rep.contents() << std::endl << std::endl;
    }


// Encode.

std::vector<uint8_t> read_reply::encode() const
  {
    ::file::Reply rep;
    
    rep.set_type(::file::READ);
    rep.set_error(error());
    rep.set_message(message());
    
    ::file::Reply_Read* read = new ::file::Reply_Read();
    read->set_contents(contents());
    
    rep.set_allocated_read(read);
    
    std::vector<uint8_t> buffer(rep.ByteSizeLong());
    rep.SerializeToArray(buffer.data(), buffer.size());
    
    return buffer;
  }

std::vector<uint8_t> read_request::encode() const
  {
    ::file::Request req;
    
    req.set_type(::file::READ);
    
    ::file::Request_Read* read = new ::file::Request_Read();
    read->set_filename(filename());
    
    req.set_allocated_read(read);
    
    std::vector<uint8_t> buffer(req.ByteSizeLong());
    req.SerializeToArray(buffer.data(), buffer.size());
    
    return buffer;
  }

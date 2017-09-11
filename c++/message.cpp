#include "message.hpp"

#include <message.pb.h>

#include <message/echo.hpp>
#include <message/read.hpp>
#include <message/write.hpp>


// Reply.

reply::
  reply(bool error, std::string const& message)
    : m_error(error)
    , m_message(message) {}

bool reply::error() const
  {
    return m_error;
  }

std::string const& reply::message() const
  {
    return m_message;
  }
  
  
// Parse.

request_base* parse_request(std::vector<uint8_t> const& buffer)
  {
    file::Request req;
    if (req.ParseFromArray(buffer.data(), buffer.size()))
      {
        if (req.has_echo())
          {
            ::file::Echo const& echo = req.echo();
            return new echo_request(echo.data());
          }
        if (req.has_read())
          {
            ::file::Request_Read const& read = req.read();
            return new read_request(read.filename());
          }
        if (req.has_write())
          {
            ::file::Request_Write const& write = req.write();
            return new write_request(write.filename(), write.contents());
          }
      }
    return nullptr;
  }

reply* parse_reply(std::vector<uint8_t> const& buffer)
  {
    file::Reply rep;
    if (rep.ParseFromArray(buffer.data(), buffer.size()))
      {
        if (rep.has_echo())
          {
            ::file::Echo const& echo = rep.echo();
            return new echo_reply(rep.error(), rep.message(), echo.data());
          }
        if (rep.has_read())
          {
            ::file::Reply_Read const& read = rep.read();
            return new read_reply(rep.error(), rep.message(), read.contents());
          }
        if (rep.has_write())
          {
            //::file::Reply_Write const& write = rep.write();
            return new write_reply(rep.error(), rep.message());
          }
      }
  }

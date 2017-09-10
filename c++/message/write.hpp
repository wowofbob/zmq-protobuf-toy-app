#ifndef _MESSAGE_WRITE_HPP_
#define _MESSAGE_WRITE_HPP_

#include <message.hpp>


struct write_reply : public reply {
  write_reply
    ( bool
    , std::string const& );
};

class write_request : public request<write_reply> {
  std::string m_filename;
  std::string m_contents;
public:
  write_request
    ( std::string const&
    , std::string const& );
  std::string const& filename() const;
  std::string const& contents() const;
  write_reply* dispatch(handler_base&);
};

struct write_handler :
  virtual public handler<write_request, write_reply>
    {
      write_reply* operator()(write_request const&);
    };
    

#endif//_MESSAGE_WRITE_HPP_

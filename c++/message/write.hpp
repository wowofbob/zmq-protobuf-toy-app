#ifndef _MESSAGE_WRITE_HPP_
#define _MESSAGE_WRITE_HPP_

#include <message.hpp>


struct write_reply : public reply {
  write_reply
    ( bool
    , std::string const& );
  std::vector<uint8_t> encode() const;
  add_dispatch(write_reply, void);
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
  std::vector<uint8_t> encode() const;
  add_dispatch(write_request, write_reply*);
};

struct write_handler :
  virtual public msg_handler<write_request, write_reply*>
    {
      write_reply* operator()(write_request const&);
    };
    
struct write_reply_handler :
  virtual public msg_handler<write_reply, void>
    {
      void operator()(write_reply const&);
    };

#endif//_MESSAGE_WRITE_HPP_

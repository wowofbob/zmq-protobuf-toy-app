#ifndef _MESSAGE_READ_HPP_
#define _MESSAGE_READ_HPP_

#include <message.hpp>


class read_reply : public reply {
  std::string m_contents;
public:
  read_reply
    ( bool
    , std::string const&
    , std::string const& );
  std::string const& contents() const;
  std::vector<uint8_t> encode() const;
  add_dispatch(read_reply, void);
};

class read_request : public request<read_reply> {
  std::string m_filename;
public:
  read_request(std::string const&);
  std::string const& filename() const;
  std::vector<uint8_t> encode() const;
  add_dispatch(read_request, read_reply*);
};

struct read_handler :
  virtual public msg_handler<read_request, read_reply*>
    {
      read_reply* operator()(read_request const&);
    };


#endif//_MESSAGE_READ_HPP_

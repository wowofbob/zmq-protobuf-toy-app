#ifndef _MESSAGE_ECHO_HPP_
#define _MESSAGE_ECHO_HPP_

#include <message.hpp>


class echo_reply : public reply {
  std::string m_data;
public:
  echo_reply
    ( bool
    , std::string const&
    , std::string const& );
  std::string const& data() const;
  std::vector<uint8_t> encode() const;
  add_dispatch(echo_reply, void);
};

class echo_request : public request<echo_reply> {
  std::string m_data;
public:
  echo_request(std::string const&);
  std::string const& data() const;
  std::vector<uint8_t> encode() const;
  add_dispatch(echo_request, echo_reply*);
};


struct echo_handler :
  virtual public msg_handler<echo_request, echo_reply*>
    {
      echo_reply* operator()(echo_request const&);
    };
    
struct echo_reply_handler :
  virtual public msg_handler<echo_reply, void>
    {
      void operator()(echo_reply const&);
    };

#endif//_MESSAGE_ECHO_HPP_

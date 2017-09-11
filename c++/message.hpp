#ifndef _MESSAGE_HPP_
#define _MESSAGE_HPP_

#include <stdint.h>

#include <string>
#include <vector>


// Encodable.
struct encodable {
  virtual std::vector<uint8_t> encode() const {};
};


struct msg_handler_base {
  virtual ~msg_handler_base() {}
};

struct msg_base {
  virtual ~msg_base() {}
};

template<class In, class Out>
struct msg_handler : public virtual msg_handler_base {
  virtual Out operator()(In const&) = 0;
  virtual ~msg_handler() {}
};

template<class Out>
class message : public msg_base {
protected:
  template<class In>
  Out dyn_dispatch(msg_handler_base& h, In const& req)
    {
      dynamic_cast<msg_handler<In, Out>&>(h)(req);
    }
public:  
  virtual ~message() {}
};


// Handler.

struct reply_base : public encodable {
  virtual void dispatch(msg_handler_base& h) = 0;
  virtual ~reply_base() {}
};

struct request_base : public encodable {
  virtual reply_base* dispatch(msg_handler_base& h) = 0;
  virtual ~request_base() {}
};

template<class Rep>
struct request : public message<Rep*>, public request_base {
  virtual ~request() {}
};


// Reply.
class reply : public message<void>, public reply_base  {
  bool        m_error;
  std::string m_message;
public:
  reply
    ( bool
    , std::string const& );
  bool               error()   const;
  std::string const& message() const;
  virtual ~reply() {}
};


// Macro tools.

#define add_dispatch(req, rep) \
  rep dispatch(msg_handler_base& h) \
    {                            \
      dyn_dispatch(h, *this);    \
    }


// Parse.

request_base* parse_request(std::vector<uint8_t> const&);
reply_base*   parse_reply  (std::vector<uint8_t> const&);


#endif//_MESSAGE_HPP_

#ifndef _MESSAGE_HPP_
#define _MESSAGE_HPP_

#include <stdint.h>

#include <string>
#include <vector>


// Encodable.
struct encodable {
  virtual std::vector<uint8_t> encode() const {};
};


// Handler.

struct handler_base {
  virtual ~handler_base() {};
};

template<class In, class Out>
struct handler : public virtual handler_base {
  virtual Out* operator()(In const&) = 0;
  virtual ~handler() {}
};


// Base request.
struct reply;
struct request_base : public encodable {
  virtual reply* dispatch(handler_base&) = 0;
  virtual ~request_base() {}
};

// Request.
template<class Rep>
class request : public request_base {
protected:
  template<class Req>
  Rep* dyn_dispatch(handler_base& h, Req const& req)
    {
      dynamic_cast<handler<Req, Rep>&>(h)(req);
    }
public:  
  virtual ~request() {}
};


// Reply.
class reply : public encodable {
  bool               m_error;
  std::string const& m_message;
public:
  reply
    ( bool
    , std::string const& );
  bool               error()   const;
  std::string const& message() const;
  virtual ~reply() {}
};


// Macro tools.

#define declare_dispatch(req, rep) \
  rep* dispatch(handler_base&);

#define define_dispatch(req, rep) \
  rep* req::dispatch(handler_base& h) \
    {                                 \
      dyn_dispatch(h, *this);         \
    }

#define add_dispatch(req, rep) \
  rep* dispatch(handler_base& h) \
    {                            \
      dyn_dispatch(h, *this);    \
    }


// Parse.

request_base* parse_request(std::vector<uint8_t> const&);
reply*        parse_reply(std::vector<uint8_t> const&);


#endif//_MESSAGE_HPP_

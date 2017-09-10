#ifndef _MESSAGE_HPP_
#define _MESSAGE_HPP_

#include <string>


// Handler.

struct handler_base {
  virtual ~handler_base() {};
};

template<class Req, class Rep>
struct handler : public virtual handler_base {
  virtual Rep* operator()(Req const&) = 0;
  virtual ~handler() {}
};


// Request.
template<class Rep>
class request {
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
class reply {
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


#endif//_MESSAGE_HPP_

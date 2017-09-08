#ifndef _MESSAGE_HPP_
#define _MESSAGE_HPP_

#include <string>
#include <iostream>

namespace msg {


  struct msg_handler_base {
    virtual ~msg_handler_base() {}
  };

  template<typename MsgType>
  struct msg_handler : public virtual msg_handler_base {
    virtual void operator()(MsgType const&) = 0;
  };


  class msg_base {

  protected:

    template<typename MsgType>
    void dyn_dispatch(msg_handler_base& h, MsgType const& msg)  
      {
        dynamic_cast<msg_handler<MsgType>&>(h)(msg);
      }  

  public:

    virtual ~msg_base() {}

  };

  
  namespace req {

    // Request base.

    struct request_base : public msg_base {
      virtual ~request_base() {}
    };

    template<typename ReqType>
    struct request_handler_base : public virtual msg_handler<ReqType> {
      virtual ~request_handler_base() {}
    };

    
    //* ECHO

    class echo_request : public request_base {
      std::string m_data;
    public:
      echo_request(std::string const&);
      std::string const& get() const;
      void dispatch(msg_handler_base&);
    };
    
    struct echo_request_handler :
      virtual public request_handler_base<echo_request>
        {
          void operator()(echo_request const&);
        };

    
    //* READ
    
    class read_request : public request_base {
      std::string m_filename;
    public:
      read_request(std::string const&);
      std::string const& get() const;
      void dispatch(msg_handler_base&);
    };
    
    struct read_request_handler :
      virtual public request_handler_base<read_request>
        {
          void operator()(read_request const&);
        };
    
    
    //* Handle everything.
    struct request_handler
      : public echo_request_handler
      , public read_request_handler {};
      
  }
}


#endif//_MESSAGE_HPP_

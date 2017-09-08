#include "message.hpp"

namespace msg {

  namespace req {

    //* ECHO
    
    echo_request::
      echo_request(std::string const& data)
        : m_data(data) {}
    
    std::string const& echo_request::get() const
      {
        return m_data;
      }
    
    void echo_request::dispatch(msg_handler_base& h)
      {
        dyn_dispatch(h, *this);
      }
    
    void echo_request_handler::
      operator()(echo_request const& req)
        {
          std::cout << "ECHO: " << req.get() << std::endl;
        }
    
    
    //* READ
    
    read_request::
      read_request(std::string const& filename)
        : m_filename(filename) {}
        
    std::string const& read_request::get() const
      {
        return m_filename;
      }
      
    void read_request::dispatch(msg_handler_base& h)
      {
        dyn_dispatch(h, *this);
      }
    
    void read_request_handler::
      operator()(read_request const& req)
        {
          std::cout << "READ: " << req.get() << std::endl;
        }
    
    
    //* WRITE
    
    
    //* LOGGER
    
  }

}

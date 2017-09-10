#include "message.hpp"


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

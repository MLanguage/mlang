#include "m_error.h"

m_error *add_error(m_error *err_list, m_error new_error) {

  if (err_list == NULL) {
    m_error *new_list = malloc(sizeof(m_error));
    if (new_list == NULL) {
      return NULL;
    }
    memcpy(new_list, &new_error, sizeof(m_error));
    return new_list;
  }

  size_t m_error_size = sizeof(m_error);
  size_t list_size = (size_t)(sizeof(err_list) / m_error_size);

  m_error *new_list = malloc(list_size + m_error_size);

  if (new_list == NULL) {
    return NULL;
  }

  memcpy(&new_list, err_list, sizeof(new_list));
  memcpy(&new_list[list_size + 1], &new_error, m_error_size);

  free(err_list);

  return new_list;
}

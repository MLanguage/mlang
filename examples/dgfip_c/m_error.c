#include "m_error.h"

int get_occurred_errors_count(m_error *errors, int size) {
  int count = 0;

  for (int i = 0; i < size; i++) {
    if (errors[i].has_occurred == true) {
      count++;
    }
  }

  return count;
}

m_error *get_occurred_errors_items(int full_list_count, m_error *full_list,
                                   m_error *filtered_errors) {
  int count = 0;

  for (int i = 0; i < full_list_count; i++) {
    if (full_list[i].has_occurred == true) {
      filtered_errors[count] = full_list[i];
      count++;
    }
  }

  return filtered_errors;
}

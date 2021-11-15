/* Copyright (C) 2021 Inria, contributor: James Barnes <bureau.si-part-ircalcul@dgfip.finances.gouv.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. */

#include "m_error.h"

int get_occurred_errors_count(m_error_occurrence* errors, int size) {
  int count = 0;

  for (int i = 0; i < size; i++) {
    if (errors[i].has_occurred == true) {
      count++;
    }
  }

  return count;
}

m_error_occurrence* get_occurred_errors_items(int full_list_count, m_error_occurrence* full_list,
                                   m_error_occurrence* filtered_errors) {
  int count = 0;

  for (int i = 0; i < full_list_count; i++) {
    if (full_list[i].has_occurred == true) {
      filtered_errors[count] = full_list[i];
      count++;
    }
  }

  return filtered_errors;
}

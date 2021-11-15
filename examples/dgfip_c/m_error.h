/* Copyright (C) 2021 Inria, contributor: James Barnes <bureau.si-part-ircalcul@dgfip.finances.gouv.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. */

#ifndef M_ERROR_
#define M_ERROR_

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum error_kind { Anomaly, Discordance, Information } error_kind;

typedef struct m_error {
  const char* kind;
  const char* major_code;
  const char* minor_code;
  const char* description;
  const char* isisf;
  const char* code_information;
} m_error;

typedef struct m_error_occurrence {
  const m_error *error;
  bool has_occurred; 
} m_error_occurrence;

int get_occurred_errors(m_error_occurrence *errors, int size);

m_error_occurrence* get_occurred_errors_items(int full_list_count, m_error_occurrence *full_list,
                                   m_error_occurrence *filtered_errors);

#endif /* M_ERROR_ */

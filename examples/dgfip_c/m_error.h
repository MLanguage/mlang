#ifndef M_ERROR_
#define M_ERROR_

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum error_kind { Anomaly, Discordance, Information } error_kind;

typedef struct m_error {
  char kind[2];
  char major_code[4];
  char minor_code[7];
  char description[81];
  char isisf[2];
} m_error;

typedef struct m_error_occurrence {
  m_error *error;
  bool has_occurred; 
} m_error_occurrence;

int get_occurred_errors(m_error_occurrence *errors, int size);

m_error_occurrence* get_occurred_errors_items(int full_list_count, m_error_occurrence *full_list,
                                   m_error_occurrence *filtered_errors);

#endif /* M_ERROR_ */

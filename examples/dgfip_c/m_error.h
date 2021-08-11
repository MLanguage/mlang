#ifndef M_ERROR_
#define M_ERROR_

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

#endif /* M_ERROR_ */

#ifndef M_ERROR_
#define M_ERROR_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum error_kind { Anomaly, Discordance, Information } error_kind;

typedef struct m_error {
  error_kind kind;
} m_error;

#endif /* M_ERROR_ */

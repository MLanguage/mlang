#include <math.h>
#include <stdbool.h>
#include <stdlib.h>

#define undefined(x) isnan(x)

typedef double m_nan_value;
// type invariant : if undefined, then value == 0

extern const m_nan_value m_nan_undefined;
extern const m_nan_value m_nan_zero;
extern const m_nan_value m_nan_one;

m_nan_value m_nan_add(m_nan_value x, m_nan_value y);
m_nan_value m_nan_sub(m_nan_value x, m_nan_value y);
m_nan_value m_nan_neg(m_nan_value x);
m_nan_value m_nan_mul(m_nan_value x, m_nan_value y);
m_nan_value m_nan_div(m_nan_value x, m_nan_value y);
m_nan_value m_nan_lt(m_nan_value x, m_nan_value y);
m_nan_value m_nan_lte(m_nan_value x, m_nan_value y);
m_nan_value m_nan_gt(m_nan_value x, m_nan_value y);
m_nan_value m_nan_gte(m_nan_value x, m_nan_value y);
m_nan_value m_nan_eq(m_nan_value x, m_nan_value y);
m_nan_value m_nan_neq(m_nan_value x, m_nan_value y);
m_nan_value m_nan_and(m_nan_value x, m_nan_value y);
m_nan_value m_nan_or(m_nan_value x, m_nan_value y);
m_nan_value m_nan_not(m_nan_value x);
m_nan_value m_nan_cond(m_nan_value c, m_nan_value t, m_nan_value f);
m_nan_value m_nan_min(m_nan_value x, m_nan_value y);
m_nan_value m_nan_max(m_nan_value x, m_nan_value y);
m_nan_value m_nan_present(m_nan_value x);
m_nan_value m_nan_null(m_nan_value x);
m_nan_value m_nan_round(m_nan_value x);
m_nan_value m_nan_floor(m_nan_value x);
bool m_nan_is_defined_true(m_nan_value x);
bool m_nan_is_defined_false(m_nan_value x);
m_nan_value m_nan_literal(double v);
m_nan_value m_nan_array_index(m_nan_value *array, m_nan_value index,
                              int array_size);
m_nan_value m_nan_multimax(m_nan_value bound, m_nan_value *array);

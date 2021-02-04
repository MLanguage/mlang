#include <math.h>
#include <stdbool.h>
#include <stdlib.h>

typedef struct m_value
{
    double value;
    bool undefined;
} m_value;
// type invariant : if undefined, then value == 0

extern const struct m_value m_undefined;
extern const struct m_value m_zero;
extern const struct m_value m_one;

m_value m_add(m_value x, m_value y);
m_value m_sub(m_value x, m_value y);
m_value m_neg(m_value x);
m_value m_mul(m_value x, m_value y);
m_value m_div(m_value x, m_value y);
m_value m_lt(m_value x, m_value y);
m_value m_lte(m_value x, m_value y);
m_value m_gt(m_value x, m_value y);
m_value m_gte(m_value x, m_value y);
m_value m_eq(m_value x, m_value y);
m_value m_neq(m_value x, m_value y);
m_value m_and(m_value x, m_value y);
m_value m_or(m_value x, m_value y);
m_value m_not(m_value x);
m_value m_cond(m_value c, m_value t, m_value f);
m_value m_min(m_value x, m_value y);
m_value m_max(m_value x, m_value y);
m_value m_present(m_value x);
m_value m_null(m_value x);
m_value m_round(m_value x);
m_value m_floor(m_value x);
bool m_is_defined_true(m_value x);
bool m_is_defined_false(m_value x);
m_value m_literal(double v);
m_value m_array_index(m_value *array, m_value index, int array_size);
m_value m_multimax(m_value bound, m_value *array);
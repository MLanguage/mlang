#include "m_nan_value.h"
#include <stdio.h>

const m_nan_value m_nan_undefined = (m_nan_value)NAN;

const m_nan_value m_nan_zero = (m_nan_value)0.0;

const m_nan_value m_nan_one = (m_nan_value)1.0;

m_nan_value m_nan_add(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) && M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else if (M_NAN_UNDEFINED(x))
     {
          return (m_nan_value)y;
     }
     else if (M_NAN_UNDEFINED(y))
     {
          return (m_nan_value)x;
     }
     else
     {
          return (m_nan_value)(x + y);
     }
}

m_nan_value m_nan_sub(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) && M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else if (M_NAN_UNDEFINED(x))
     {
          return (m_nan_value)(-y);
     }
     else if (M_NAN_UNDEFINED(y))
     {
          return (m_nan_value)x;
     }
     else
     {
          return (m_nan_value)(x - y);
     }
}

m_nan_value m_nan_neg(m_nan_value x)
{
     return -x;
}

m_nan_value m_nan_mul(m_nan_value x, m_nan_value y)
{
     return x * y;
}

m_nan_value m_nan_div(m_nan_value x, m_nan_value y)
{
     if (y == 0.0)
     {
          return m_nan_zero;
     }
     else
     {
          return (m_nan_value)(x / y);
     }
}

m_nan_value m_nan_lt(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) || M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else
     {
          return (m_nan_value)(x < y);
     }
}

m_nan_value m_nan_lte(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) || M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else
     {
          return (m_nan_value)(x <= y);
     }
}

m_nan_value m_nan_gt(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) || M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else
     {
          return (m_nan_value)(x > y);
     }
}

m_nan_value m_nan_gte(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) || M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else
     {
          return (m_nan_value)(x >= y);
     }
}

m_nan_value m_nan_eq(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) || M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else
     {
          return (m_nan_value)(x == y);
     }
}

m_nan_value m_nan_neq(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) || M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else
     {
          return (m_nan_value)(x != y);
     }
}

m_nan_value m_nan_and(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) || M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else
     {
          return (m_nan_value)(x && y);
     }
}

m_nan_value m_nan_or(m_nan_value x, m_nan_value y)
{
     if (M_NAN_UNDEFINED(x) && M_NAN_UNDEFINED(y))
     {
          return m_nan_undefined;
     }
     else if (M_NAN_UNDEFINED(x))
     {
          return (m_nan_value)y;
     }
     else if (M_NAN_UNDEFINED(y))
     {
          return (m_nan_value)x;
     }
     else
     {
          return (m_nan_value)(x || y);
     }
}

m_nan_value m_nan_not(m_nan_value x)
{
     return (m_nan_value)(!x);
}

m_nan_value m_nan_cond(m_nan_value c, m_nan_value t, m_nan_value f)
{
     if (M_NAN_UNDEFINED(c))
     {
          return m_nan_undefined;
     }
     else
     {
          if (c)
          {
               return t;
          }
          else
          {
               return f;
          }
     }
}

m_nan_value m_nan_max(m_nan_value x, m_nan_value y)
{
     return (m_nan_value)fmax(M_NAN_UNDEFINED(x) ? 0.0 : x, M_NAN_UNDEFINED(y) ? 0.0 : y);
}

m_nan_value m_nan_min(m_nan_value x, m_nan_value y)
{
     return (m_nan_value)fmin(M_NAN_UNDEFINED(x) ? 0.0 : x, M_NAN_UNDEFINED(y) ? 0.0 : y);
}

m_nan_value m_nan_present(m_nan_value x)
{
     if (M_NAN_UNDEFINED(x))
     {
          return m_nan_zero;
     }
     else
     {
          return m_nan_one;
     }
}

m_nan_value m_nan_null(m_nan_value x)
{
     if (M_NAN_UNDEFINED(x))
     {
          return m_nan_one;
     }
     else
     {
          return m_nan_zero;
     }
}

m_nan_value m_nan_round(m_nan_value x)
{
     if (M_NAN_UNDEFINED(x))
     {
          return m_nan_undefined;
     }
     else
     {
          double tmp = x + (x < 0 ? -0.50005 : 0.50005);
          return (m_nan_value)(__int64_t)tmp;
     }
}

m_nan_value m_nan_floor(m_nan_value x)
{
     return (m_nan_value)floor(x + 0.000001);
}

bool m_nan_is_defined_true(m_nan_value x)
{
     if (M_NAN_UNDEFINED(x))
     {
          return false;
     }
     else
     {
          return x != 0.0;
     }
}

bool m_nan_is_defined_false(m_nan_value x)
{
     if (M_NAN_UNDEFINED(x))
     {
          return false;
     }
     else
     {
          return x == 0.0;
     }
}

m_nan_value m_nan_literal(double v)
{
     return (m_nan_value)v;
}

m_nan_value m_nan_array_index(m_nan_value *array, m_nan_value index, int array_size)
{
     if (M_NAN_UNDEFINED(index))
     {
          return m_nan_undefined;
     }
     else
     {
          if (index < 0)
          {
               return m_nan_zero;
          }
          else if (index >= array_size - 1)
          {
               return m_nan_undefined;
          }
          else
          {
               return array[(int)floor(index)];
          }
     }
}

m_nan_value m_nan_multimax(m_nan_value bound, m_nan_value *array)
{
     if (M_NAN_UNDEFINED(bound))
     {
          printf("Multimax bound undefined!");
          exit(-1);
     }
     else
     {
          int max_index = floor(bound);
          m_nan_value max = m_nan_add(array[0], m_nan_zero);
          for (int i = 0; i <= max_index; i++)
          {
               m_nan_value challenger = m_nan_add(array[i], m_nan_zero);
               if (challenger > max)
               {
                    max = challenger;
               }
          }
          return max;
     }
}

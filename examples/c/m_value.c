#include "m_value.h"
#include <stdio.h>

const struct m_value m_undefined = (struct m_value){.value = 0, .undefined = true};

const struct m_value m_zero = (struct m_value){.value = 0, .undefined = false};

const struct m_value m_one = (struct m_value){.value = 1, .undefined = false};

m_value m_add(m_value x, m_value y)
{
    if (x.undefined && y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value + y.value, .undefined = false};
    }
}

m_value m_sub(m_value x, m_value y)
{
    if (x.undefined && y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value - y.value, .undefined = false};
    }
}

m_value m_neg(m_value x)
{
    if (x.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = -x.value, .undefined = false};
    }
}

m_value m_mul(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value * y.value, .undefined = false};
    }
}

m_value m_div(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else if (y.value == 0)
    {
        return m_zero;
    }
    else
    {
        return (struct m_value){.value = x.value / y.value, .undefined = false};
    }
}

m_value m_lt(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value < y.value, .undefined = false};
    }
}

m_value m_lte(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value <= y.value, .undefined = false};
    }
}

m_value m_gt(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value > y.value, .undefined = false};
    }
}

m_value m_gte(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value >= y.value, .undefined = false};
    }
}

m_value m_eq(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value == y.value, .undefined = false};
    }
}

m_value m_neq(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value != y.value, .undefined = false};
    }
}

m_value m_and(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value && y.value, .undefined = false};
    }
}

m_value m_or(m_value x, m_value y)
{
    if (x.undefined || y.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = x.value || y.value, .undefined = false};
    }
}

m_value m_not(m_value x)
{
    if (x.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = !x.value, .undefined = false};
    }
}

m_value m_cond(m_value c, m_value t, m_value f)
{
    if (c.undefined)
    {
        return m_undefined;
    }
    else
    {
        if (c.value)
        {
            return t;
        }
        else
        {
            return f;
        }
    }
}

m_value m_max(m_value x, m_value y)
{
    return (struct m_value){
        .value = fmax(x.value, y.value),
        .undefined = false};
}

m_value m_min(m_value x, m_value y)
{
    return (struct m_value){
        .value = fmin(x.value, y.value),
        .undefined = false};
}

m_value m_present(m_value x)
{
    if (x.undefined)
    {
        return m_zero;
    }
    else
    {
        return m_one;
    }
}

m_value m_null(m_value x)
{
    if (x.undefined)
    {
        return m_one;
    }
    else
    {
        return m_zero;
    }
}

m_value m_round(m_value x)
{
    if (x.undefined)
    {
        return m_undefined;
    }
    else
    {
        double tmp = x.value + (x.value < 0 ? -0.50005 : 0.50005);
        return (struct m_value){.value = (double)(__int64_t) tmp, .undefined = false};
    }
}

m_value m_floor(m_value x)
{
    if (x.undefined)
    {
        return m_undefined;
    }
    else
    {
        return (struct m_value){.value = floor(x.value + 0.000001), .undefined = false};
    }
}

bool m_is_defined_true(m_value x)
{
    if (x.undefined)
    {
        return false;
    }
    else
    {
        return x.value != 0;
    }
}

bool m_is_defined_false(m_value x)
{
    if (x.undefined)
    {
        return false;
    }
    else
    {
        return x.value == 0;
    }
}

m_value m_literal(double v)
{
    return (struct m_value){.value = v, .undefined = false};
}

m_value m_array_index(m_value *array, m_value index, int array_size)
{
    if (index.undefined)
    {
        return m_undefined;
    }
    else
    {
        if (index.value < 0)
        {
            return m_zero;
        }
        else if (index.value >= array_size - 1)
        {
            return m_undefined;
        }
        else
        {
            return array[(int)floor(index.value)];
        }
    }
}

m_value m_multimax(m_value bound, m_value *array)
{
    if (bound.undefined)
    {
        printf("Multimax bound undefined!");
        exit(-1);
    }
    else
    {
        int max_index = floor(bound.value);
        m_value max = m_add(array[0], m_zero);
        for (int i = 0; i <= max_index; i++)
        {
            m_value challenger = m_add(array[i], m_zero);
            if (challenger.value > max.value)
            {
                max = challenger;
            }
        }
        return max;
    }
}
type m_value = { undefined : bool; value : float }

type m_array = m_value array

type m_context = m_value list

let m_undef : m_value = { undefined = true; value = 0.0 }

let m_zero : m_value = { undefined = false; value = 0.0 }

let m_one : m_value = { undefined = false; value = 1.0 }

let m_add (x : m_value) (y : m_value) : m_value =
  if x.undefined && y.undefined then m_undef
  else { undefined = false; value = x.value +. y.value }

let m_multiply (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else { undefined = false; value = x.value *. y.value }

let m_subtract (x : m_value) (y : m_value) : m_value =
  if x.undefined && y.undefined then m_undef
  else { undefined = false; value = x.value -. y.value }

let m_divide (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else
    {
      undefined = false;
      value = (if y.value = 0.0 then 0.0 else x.value /. y.value);
    }

let m_and (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else if x.value <> 0.0 && y.value <> 0.0 then m_one
  else m_zero

let m_or (x : m_value) (y : m_value) : m_value =
  if x.undefined && y.undefined then m_undef
  else if x.value <> 0.0 || y.value <> 0.0 then m_one
  else m_zero

let m_cond (condition : m_value) (true_value : m_value) (false_value : m_value)
    : m_value =
  match condition with
  | { undefined = true; value = _ } -> m_undef
  | { undefined = false; value = 0.0 } -> false_value
  | { undefined = false; value = _ } -> true_value

let m_greater_than (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else if x.value > y.value then m_one
  else m_zero

let m_greater_than_equal (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else if x.value >= y.value then m_one
  else m_zero

let m_less_than (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else if x.value < y.value then m_one
  else m_zero

let m_less_than_equal (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else if x.value <= y.value then m_one
  else m_zero

let m_equal (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else if x.value = y.value then m_one
  else m_zero

let m_not_equal (x : m_value) (y : m_value) : m_value =
  if x.undefined || y.undefined then m_undef
  else if x.value <> y.value then m_one
  else m_zero

let m_not (x : m_value) : m_value =
  if x.undefined then m_undef else if x.value = 0.0 then m_one else m_zero

let m_neg (x : m_value) : m_value =
  if x.undefined then m_undef
  else { undefined = true; value = Float.neg x.value }

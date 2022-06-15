type m_value = { undefined : bool; value : float }

type m_array = m_value array

type m_context = { tgv : m_array; local_variables : m_array }

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

let m_table_value_at_index (variable_array : m_array) (table_start : int)
    (index : m_value) (size : int) =
  if index.undefined then m_undef
  else
    let offset = int_of_float index.value in
    match offset with
    | x when x < 0 -> m_zero
    | x when x > size -> m_undef
    | _ -> Array.get variable_array (offset + table_start)

let m_max (x : m_value) (y : m_value) : m_value =
  { undefined = false; value = max x.value y.value }

let m_min (x : m_value) (y : m_value) : m_value =
  { undefined = false; value = min x.value y.value }

let m_round (x : m_value) : m_value =
  if x.undefined then m_undef
  else
    {
      undefined = false;
      value =
        floor (if x.value < 0.0 then x.value -. 0.50005 else x.value +. 0.50005);
    }

let m_null = m_not

let m_floor (x : m_value) : m_value =
  if x.undefined then m_undef
  else { undefined = false; value = floor (x.value +. 0.000001) }

let m_present (x : m_value) : m_value = if x.undefined then m_zero else m_one

let m_multimax (bound_variable : m_value) (variable_array : m_array)
    (position : int) : m_value =
  if bound_variable.undefined then failwith "Multimax bound undefined!"
  else
    let bound = int_of_float bound_variable.value in
    let get_position_value_or_zero position =
      m_add (Array.get variable_array position) m_zero
    in
    let rec multimax variable_array current_index max_index reference =
      let new_max =
        m_max reference (get_position_value_or_zero current_index)
      in
      if current_index = max_index then new_max
      else multimax variable_array (current_index + 1) max_index new_max
    in
    if bound >= 1 then
      multimax variable_array (position + 1) (position + bound)
        (get_position_value_or_zero position)
    else get_position_value_or_zero position

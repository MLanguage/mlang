module type T = SetSetExt.T with type base_elt = string and type elt = StrSet.t

include T

include Map.Make (StrSet)

module type T = Map.S with type key = StrSet.t

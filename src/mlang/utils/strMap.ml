include Map.Make (String)

module type T = Map.S with type key = String.t

module type T = MapExt.T with type key = string

include T

val keySet : 'a t -> StrSet.t

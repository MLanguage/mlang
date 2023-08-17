module type T = MapExt.T with type key = StrSet.t

include T

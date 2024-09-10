module type T = MapExt.T with type key = IntSet.t

include T

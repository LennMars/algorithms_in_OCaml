type t
val r : int * int -> t
val ( +^ ) : t -> t -> t
val ( -^ ) : t -> t -> t
val ( *^ ) : t -> t -> t
val ( /^ ) : t -> t -> t
val ( >^ ) : t -> t -> bool
val ( >=^ ) : t -> t -> bool
val ( <^ ) : t -> t -> bool
val ( <=^ ) : t -> t -> bool
val ( =^ ) : t -> t -> bool
val ( <>^ ) : t -> t -> bool
val one : t
val zero : t
val negative : t -> t
val inverse : t -> t
val float_of_rational : t -> float
val rational_of_int : int -> t

(** Extended Pervasives module. *)

val identity : 'a -> 'a
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( <| ) : ('a -> 'b) -> 'a -> 'b
val swap : 'a * 'b -> 'b * 'a
val swap_arg : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val maxf : ('a -> 'b) -> 'a -> 'a -> 'b
val xor : bool -> bool -> bool
val incr : int ref -> unit
val triple1 : 'a * 'b * 'c -> 'a
val triple2 : 'a * 'b * 'c -> 'b
val triple3 : 'a * 'b * 'c -> 'c
val pi : float
val iter : int -> ('a -> 'b) -> 'a -> unit
val time : (unit -> float) -> int -> ('a -> 'b) -> 'a -> float
val general_exp : 'a -> ('a -> 'a -> 'a) -> 'a -> int -> 'a
val int_exp : int -> int -> int
val minimum_bigger_power_of_two : int -> int
val string_of_option : ('a -> string) -> 'a option -> string
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val add_list : t -> int list -> t
    val print : t -> unit
  end
module Queue2 :
  sig
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val singleton : 'a -> 'a t
    val push : 'a -> 'a t -> 'a t
    val pop : 'a t -> 'a * 'a t
    val peek : 'a t -> 'a
  end

(*
    reference : Okasaki, C. (1995) Simple and Efficient Purely Functional Queues and Deques. J. Functional Programming, 5(4), 583–592.
 *)
module Stack2 :
  sig
    type 'a t
    val empty : 'a t
    val singleton : 'a -> 'a t
    val is_empty : 'a t -> bool
    val length : 'a t -> int
    val push : 'a -> 'a t -> 'a t
    val pop : 'a t -> 'a * 'a t
    val peek : 'a t -> 'a
  end
module LazyList :
  sig
    type 'a t = Cons of 'a * 'a t lazy_t
    val from : int -> int t
    val head : 'a t -> 'a
    val tail : 'a t -> 'a t
    val take : int -> 'a t -> 'a list
    val map : ('a -> 'b) -> 'a t -> 'b t
    val nth : int -> 'a t -> 'a
    val sift : int -> int t -> int t
    val sieve : int t -> int t
    val primes : int t
  end

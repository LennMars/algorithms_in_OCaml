(** Extended Array Module. *)

(** Print elements in an array. Function which converts an element to some string representation is needed. *)
val print : ('a -> string) -> 'a array -> unit

val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

(** Swap two elements at the designated locations. destructive. *)
val swap : int -> int -> 'a array -> unit

(** Scan an array from left and return the index of the first element which satisfies cond. *)
val find_num_left : ?start:int -> ('a -> bool) -> 'a array -> int

(** Same to find_num_left, but scanning starts from right. *)
val find_num_right : ?start:int -> ('a -> bool) -> 'a array -> int

(** Split array at just left the nth element. *)
val split : int -> 'a array -> 'a array * 'a array

(** Same to that of List *)
val for_all : ('a -> bool) -> 'a array -> bool
val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool

(** Same to that of List *)
val exists : ('a -> bool) -> 'a array -> bool
val permutate : int list -> 'a array -> 'a array

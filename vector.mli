type 'a t
exception Empty
val create : unit -> 'a t
val length : 'a t -> int
val capacity : 'a t -> int
val push : 'a t -> 'a -> unit
val contains : 'a t -> 'a -> bool
val for_all : ('a -> bool) -> 'a t -> bool
val push_if_unique : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val pop_n : 'a t -> int -> unit
val iter : ('a -> 'b) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val map_to_array : ('a -> 'b) -> 'a t -> 'b array
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val member : 'a -> 'a t -> bool
val memq : 'a -> 'a t -> bool
val clear : 'a t -> unit
val purge : 'a t -> unit
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val unsafe_get : 'a t -> int -> 'a
val unsafe_set : 'a t -> int -> 'a -> unit
val find : ('a -> bool) -> 'a t -> int
val remove_if : ('a -> bool) -> 'a t -> unit
val sub : 'a t -> int -> int -> 'a array
val insert : 'a t -> 'a -> int -> unit
val erase : 'a t -> int -> int -> unit
val append : 'a t -> 'a t -> unit
val append_array : 'a t -> 'a array -> unit
val sort : ('a -> 'a -> int) -> 'a t -> unit
val to_array : 'a t -> 'a array
val of_array : 'a array -> 'a t
val of_list : 'a list -> 'a t
val front : 'a t -> 'a
val back : 'a t -> 'a
val append_map : 'a t -> ('b -> 'a) -> 'b t -> unit
val partition : 'a t -> ('a -> bool) -> int
val range : int -> int -> int t
val push_ordered : 'a t -> ('a -> 'a -> bool) -> 'a -> unit
val is_sorted : 'a t -> ('a -> 'a -> bool) -> bool
val any : ('a -> bool) -> 'a t -> bool

(** This module extends some modules of the standard library. *)

module List = struct

  include List

  (** [index_of p l] returns the index of the first element [x] of [l]
      such [p x = true]. Raise [Not_found] otherwise. *)
  let index_of : ('a -> bool) -> 'a list -> int =
    fun p l ->
      let rec aux i = function
        | [] -> raise Not_found
        | x :: xs -> if p x then i else aux (succ i) xs
      in
      aux 0 l

end

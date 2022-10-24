

module type MonoMapS =
  sig
    type key
    (** The type of the map keys. *)

    type rng
    (** The type of the map range. *)

    type t
    (** The type of maps from type [key] to type [rng]. *)

    val empty: t
    (** The empty map. *)

    val is_empty: t -> bool
    (** Test whether a map is empty or not. *)

    val mem: key -> t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)
(*
    val add: key -> rng -> t -> t
    (** [add key data m] returns a map containing the same bindings as
       [m], plus a binding of [key] to [data]. If [key] was already bound
       in [m] to a value that is physically equal to [data],
       [m] is returned unchanged (the result of the function is
       then physically equal to [m]). Otherwise, the previous binding
       of [key] in [m] disappears.
       @before 4.03 Physical equality was not ensured. *)

    val update: key -> (rng option -> rng option) -> t -> t
    (** [update key f m] returns a map containing the same bindings as
        [m], except for the binding of [key]. Depending on the value of
        [y] where [y] is [f (find_opt key m)], the binding of [key] is
        added, removed or updated. If [y] is [None], the binding is
        removed if it exists; otherwise, if [y] is [Some z] then [key]
        is associated to [z] in the resulting map.  If [key] was already
        bound in [m] to a value that is physically equal to [z], [m]
        is returned unchanged (the result of the function is then
        physically equal to [m]).
        @since 4.06.0
    *)

    val singleton: key -> rng -> t
    (** [singleton x y] returns the one-element map that contains a binding
        [y] for [x].
        @since 3.12.0
     *)
     *)
    val remove: key -> t -> t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map.
       If [x] was not in [m], [m] is returned unchanged
       (the result of the function is then physically equal to [m]).
       @before 4.03 Physical equality was not ensured. *)
(*
    val union: (key -> rng -> rng -> rng option) -> t -> t -> t
    (** [union f m1 m2] computes a map whose keys are a subset of the keys
        of [m1] and of [m2].  When the same binding is defined in both
        arguments, the function [f] is used to combine them.
        This is a special case of [merge]: [union f m1 m2] is equivalent
        to [merge f' m1 m2], where
        - [f' _key None None = None]
        - [f' _key (Some v) None = Some v]
        - [f' _key None (Some v) = Some v]
        - [f' key (Some v1) (Some v2) = f key v1 v2]

        @since 4.03.0
    *)

    val compare: (rng -> rng -> int) -> t -> t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: (rng -> rng -> bool) -> t -> t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)

    val iter: (key -> rng -> unit) -> t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: (key -> rng -> 'b -> 'b) -> t -> 'b -> 'b
    (** [fold f m init] computes [(f kN dN ... (f k1 d1 init)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val for_all: (key -> rng -> bool) -> t -> bool
    (** [for_all f m] checks if all the bindings of the map
        satisfy the predicate [f].
        @since 3.12.0
     *)

    val exists: (key -> rng -> bool) -> t -> bool
    (** [exists f m] checks if at least one binding of the map
        satisfies the predicate [f].
        @since 3.12.0
     *)

    val filter: (key -> rng -> bool) -> t -> t
    (** [filter f m] returns the map with all the bindings in [m]
        that satisfy predicate [p]. If every binding in [m] satisfies [f],
        [m] is returned unchanged (the result of the function is then
        physically equal to [m])
        @since 3.12.0
       @before 4.03 Physical equality was not ensured.
     *)

    val partition: (key -> rng -> bool) -> t -> t * t
    (** [partition f m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [m] that satisfy the
        predicate [f], and [m2] is the map with all the bindings of
        [m] that do not satisfy [f].
        @since 3.12.0
     *)

    val cardinal: t -> int
    (** Return the number of bindings of a map.
        @since 3.12.0
     *)
     *)
    val bindings: t -> (key * rng) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order of keys with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Stdlib.Map.Make}.
        @since 3.12.0
     *)
(*
    val min_binding: t -> (key * rng)
    (** Return the binding with the smallest key in a given map
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the map is empty.
        @since 3.12.0
     *)

    val min_binding_opt: t -> (key * rng) option
    (** Return the binding with the smallest key in the given map
       (with respect to the [Ord.compare] ordering), or [None]
       if the map is empty.
        @since 4.05
     *)

    val max_binding: t -> (key * rng)
    (** Same as {!min_binding}, but returns the binding with
        the largest key in the given map.
        @since 3.12.0
     *)

    val max_binding_opt: t -> (key * rng) option
    (** Same as {!min_binding_opt}, but returns the binding with
        the largest key in the given map.
        @since 4.05
     *)

    val choose: t -> (key * rng)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 3.12.0
     *)

    val choose_opt: t -> (key * rng) option
    (** Return one binding of the given map, or [None] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 4.05
     *)

    val split: key -> t -> t * rng option * t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)
     *)
    val find: key -> t -> rng
    (** [find x m] returns the current value of [x] in [m],
       or raises [Not_found] if no binding for [x] exists. *)
(*
    val find_opt: key -> t -> rng option
    (** [find_opt x m] returns [Some v] if the current value of [x]
        in [m] is [v], or [None] if no binding for [x] exists.
        @since 4.05
    *)

    val find_first: (key -> bool) -> t -> key * rng
    (** [find_first f m], where [f] is a monotonically increasing function,
       returns the binding of [m] with the lowest key [k] such that [f k],
       or raises [Not_found] if no such key exists.

       For example, [find_first (fun k -> Ord.compare k x >= 0) m] will return
       the first binding [k, v] of [m] where [Ord.compare k x >= 0]
       (intuitively: [k >= x]), or raise [Not_found] if [x] is greater than
       any element of [m].

        @since 4.05
       *)

    val find_first_opt: (key -> bool) -> t -> (key * rng) option
    (** [find_first_opt f m], where [f] is a monotonically increasing
       function, returns an option containing the binding of [m] with the
       lowest key [k] such that [f k], or [None] if no such key exists.
        @since 4.05
       *)

    val find_last: (key -> bool) -> t -> key * rng
    (** [find_last f m], where [f] is a monotonically decreasing function,
       returns the binding of [m] with the highest key [k] such that [f k],
       or raises [Not_found] if no such key exists.
        @since 4.05
       *)

    val find_last_opt: (key -> bool) -> t -> (key * rng) option
    (** [find_last_opt f m], where [f] is a monotonically decreasing
       function, returns an option containing the binding of [m] with
       the highest key [k] such that [f k], or [None] if no such key
       exists.
        @since 4.05
       *)

    (** {1 Maps and Sequences} *)

    val to_seq : t -> (key * rng) Seq.t
    (** Iterate on the whole map, in ascending order of keys
        @since 4.07 *)

    val to_rev_seq : t -> (key * rng) Seq.t
    (** Iterate on the whole map, in descending order of keys
        @since 4.12 *)

    val to_seq_from : key -> t -> (key * rng) Seq.t
    (** [to_seq_from k m] iterates on a subset of the bindings of [m],
        in ascending order of keys, from key [k] or above.
        @since 4.07 *)

    val add_seq : (key * rng) Seq.t -> t -> t
    (** Add the given bindings to the map, in order.
        @since 4.07 *)

    val of_seq : (key * rng) Seq.t -> t
    (** Build a map from the given bindings
        @since 4.07 *)
 *)
  end

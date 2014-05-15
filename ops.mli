(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module type OpsBase = sig
    type t
    val zero : t
    val one : t
    val (+:) : t -> t -> t
    val (-:) : t -> t -> t
    val ( *: ) : t -> t -> t
    val (/:) : t -> t -> t
    val to_string : t -> string
end

module type OpsFull = sig
    include OpsBase

    (* arithmetic *)
    val (%:) : t -> t -> t
    val abs : t -> t

    (* shift *)

    val (<<:) : t -> int -> t
    val (>>+) : t -> int -> t
    val (>>:) : t -> int -> t

    (* bitwise *)
        
    val (&:) : t -> t -> t
    val (|:) : t -> t -> t
    val (^:) : t -> t -> t
    val (~:) : t -> t

    (* conversion *)

    val of_int : int -> t
    val to_int : t -> int
    val of_int32 : int32 -> t
    val to_int32 : t -> int32
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val of_float : float -> t
    val to_float : t -> float
    val of_string : string -> t
end

module Int : (OpsFull with type t = int)
module Int32 : (OpsFull with type t = int32)
module Int64 : (OpsFull with type t = int64)
module Float : (OpsFull with type t = float)



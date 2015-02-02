(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)


module type S = sig

    type t 
    type matrix = t array array

    (* size of matrix *)
    val rows : matrix -> int
    val cols : matrix -> int

    (*val print : ?c:out_channel -> (t -> string) -> int -> matrix -> unit*)

    (* construction of various matrices *)
    val init : int -> int -> (int -> int -> t) -> matrix
    val create : int -> int -> matrix
    val copy : matrix -> matrix
    val identity : int -> matrix
    val transpose : matrix -> matrix
    val map : (t -> t) -> matrix -> matrix
    val map2 : (t -> t -> t) -> matrix -> matrix -> matrix

    val row_vector : t array -> matrix
    val col_vector : t array -> matrix

    (* concatenate matrices *)
    val (>>) : matrix -> matrix -> matrix
    val (^^) : matrix -> matrix -> matrix

    (* select a sub matrix *)
    val sub : int -> int -> int -> int -> matrix -> matrix

    (* arithmetic *)
    val (+:) : matrix -> matrix -> matrix
    val (-:) : matrix -> matrix -> matrix
    val ( *: ) : matrix -> matrix -> matrix
    val ( *:. ) : matrix -> t -> matrix

    (* functions related to inverses *)
    val minor : int -> int -> matrix -> matrix
    val det : matrix -> t
    val adjoint_inverse : matrix -> t * matrix
    
    (* these functions require element division *)
    val gauss_jordan : matrix -> matrix
    val gauss_jordan_inverse : matrix -> matrix

    (* elementary row operations *)
    module Row : sig
        val swap : int -> int -> int -> matrix
        val mult : int -> int -> t -> matrix
        val madd : int -> int -> int -> t -> matrix
    end

end

module Make(Ops : Ops.OpsBase) : (S with type t = Ops.t)


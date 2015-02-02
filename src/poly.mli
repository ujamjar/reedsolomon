(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** basic polynomials of 1 variable *)
module type S = sig

    (** polynomial coefficient type *)
    type elt
    (** an array of 'elt's representing the polynomial.  powers
        are at index position (ie lowest first) *)
    type t 

    (** the degree of the polynomial *)
    val degree : t -> int
    (** this is represented as [|E.zero|] *)
    val zero : t
    (** this is represented as [|E.one|] *)
    val one : t
    (** this is represented as [|E.zero;E.one|] *)
    val x : t
    (** convert to poly *)
    val to_poly : elt array -> t
    (** convert from poly *)
    val of_poly : t -> elt array
    (** make a copy of the poly *)
    val copy : t -> t

    (** control over print formatting *)
    type poly_format = 
        {
            coef : elt -> string;
            indet : int -> string;
        }
    val poly_format : poly_format
    val string_format : bool -> poly_format -> int -> elt -> string

    (** create string of poly *)
    val to_string : ?down:bool -> ?str:(int -> elt -> string) -> t -> string

    (** legalise the poly.  high order powers which are 0 are removed. *)
    val trim : t -> t
 
    val slice : t -> int -> t
       
    (** poly addition *)
    val (+:) : t -> t -> t
    (** poly subtraction *)
    val (-:) : t -> t -> t
    (** poly multiplication *)
    val ( *: ) : t -> t -> t
    (** poly division *)
    val (/:) : t -> t -> t * t
    (** scalar multiplication *)
    val ( *:. ) : t -> elt -> t
    (** scalar division *)
    val (/:.) : t -> elt -> t
    (** multiply poly by x^n *)
    val (^:) : t -> int -> t
    (** raise poly to power n *)
    val ( **: ) : t -> int -> t
    
    (** extended gcd algorithm *)
    val ext_gcd : t -> t -> t * t

    (** evaluate polynomial *)
    val eval : t -> elt -> elt

end 

(** Basic polynomial representations.  Coefficients are members of
    Ops.OpsBase (which effectively provide '+' and '*') *)
module Make(E : Ops.OpsBase) : (S with type t = E.t array
                                      and type elt = E.t)


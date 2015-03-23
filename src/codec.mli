(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(** Configuration of a Reed-Solomon code *)
module type RsParams = 
sig
  val k : int
  val t : int
  val b : int
end

(** RS encoding and decoding *)
module type RsPoly = sig

  type elt 

  module M : (Matrix.S with type t = elt)
  module R : (Poly.S with type t = elt array and type elt = elt)

  type poly = R.t
  type loc = int

  val root : int -> elt
  val generator : poly
  val xn : int -> poly
  val x2t : poly
  val parity : poly -> poly
  val encode : poly -> poly

  val horner : poly -> elt -> elt
  val syndromes : poly -> poly

  val key_equations : poly -> int -> M.matrix * M.matrix
  val solve_key_equations : M.matrix * M.matrix -> M.matrix
  val peterson : poly -> poly

  val euclid_inner : (poly * poly) -> (poly * poly) -> (poly * poly)
  val euclid : ?norm:bool -> ?lim:int -> poly -> poly * poly

  val berlekamp_massey_iter : 
    poly -> int -> (poly * poly * int) -> (poly * poly * int)
  val berlekamp_massey : poly -> poly

  (** inversionless berlekamp massey algorithms.

      Based on the paper "High-speed architectures for
      Reed-Solomon decoders" Dilip V Sarwate, Naresh R Shanbhag *)
  module Sarwate : sig
    val iBM : poly -> poly
    val riBM : poly -> poly * poly
    val rriBM : poly -> poly * poly
    val forney : poly -> poly -> (loc -> elt)
  end

  val chien : poly -> loc list
  val error_location : loc -> int

  val error_magnitude : int -> poly -> poly -> poly
  val deriv : poly -> poly
  val forney : poly -> poly -> (loc -> elt)

  val error : elt list -> loc list -> poly

  val correct : poly -> poly -> poly

  val decode_euclid : poly -> poly
  val decode_berlekamp_massey : poly -> poly
  val decode_peterson : poly -> poly
  val decode : poly -> poly

  val erasure_locator : int list -> poly
  val zero_erasures : poly -> int list -> poly
  val error_and_erasure : elt list -> loc list -> elt list -> loc list -> poly

  val decode_erasures_euclid : poly -> int list -> poly
  val decode_erasures : poly -> int list -> poly

  val decode_errors_and_erasures_euclid : poly -> int list -> poly
  (* erasure decoding not working with berlekamp massey *)
  (*val decode_errors_and_erasures_berlekamp_massey : poly -> int list -> poly*)
  val decode_errors_and_erasures : poly -> int list -> poly
end

(** Create a Reed-Solomon code based on the given Galois field and code parameters *)
module MakePoly(G : Galois.Table.Ops)(P : RsParams) : 
  (RsPoly with type elt = G.t)

(* some example RS CODECs in use *)

module type Standard = sig
  module Gp : Galois.Table.Params
  module G : Galois.Table.Ops with type t = int
  module Rp : RsParams
  module R : RsPoly with type elt = int
end

module MakeStandard(Gp : Galois.Table.Params)(Rp : RsParams) : Standard

(** Test code used in BBC white paper *)
module BBCTest : Standard

(** Consultative Committee for Space Data Systems *)
module CCSDS : sig
  val dual_of_poly : int array
  val poly_of_dual : int array
  (** t=16 *)
  module Rs16 : Standard
  (** t=8 *)
  module Rs8  : Standard
end

(** Digital Video Broadcasting *)
module DVB : Standard

(** Advanced Television Systems Committee *)
module ATSC : Standard

(** Interfaces for the Optical Transport Network *)
module G709 : Standard



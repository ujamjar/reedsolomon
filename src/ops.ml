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

  val (%:) : t -> t -> t
  val abs : t -> t

  val (<<:) : t -> int -> t
  val (>>+) : t -> int -> t
  val (>>:) : t -> int -> t

  val (&:) : t -> t -> t
  val (|:) : t -> t -> t
  val (^:) : t -> t -> t
  val (~:) : t -> t

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

module Int = struct
  type t = int
  let zero = 0
  let one = 1
  let (+:) = (+)
  let (-:) = (-)
  let ( *: ) = ( * )
  let (/:) = (/)
  let (%:) = (mod)
  let abs = abs
  let (<<:) = (lsl)
  let (>>+) = (asr)
  let (>>:) = (lsr)
  let (&:) = (land)
  let (|:) = (lor)
  let (^:) = (lxor)
  let (~:) = lnot
  let of_int x = x
  let to_int x = x
  let of_int32 = Int32.to_int
  let to_int32 = Int32.of_int
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let of_float x = int_of_float x
  let to_float x = float_of_int x
  let of_string x = int_of_string x
  let to_string x = string_of_int x
end

module Int32 = struct
  type t = int32
  open Int32
  let zero = 0l
  let one = 1l
  let (+:) = add
  let (-:) = sub
  let ( *: ) = mul
  let (/:) = div
  let (%:) = rem
  let abs = abs
  let (<<:) = shift_left
  let (>>+) = shift_right
  let (>>:) = shift_right_logical
  let (&:) = logand
  let (|:) = logor
  let (^:) = logxor
  let (~:) = lognot
  let of_int = of_int
  let to_int = to_int
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
  let of_float = of_float 
  let to_float = to_float
  let of_string = of_string 
  let to_string = to_string 
end

module Int64 = struct
  type t = int64
  open Int64
  let zero = 0L
  let one = 1L
  let (+:) = add
  let (-:) = sub
  let ( *: ) = mul
  let (/:) = div
  let (%:) = rem
  let abs = abs
  let (<<:) = shift_left
  let (>>+) = shift_right
  let (>>:) = shift_right_logical
  let (&:) = logand
  let (|:) = logor
  let (^:) = logxor
  let (~:) = lognot
  let of_int = of_int
  let to_int = to_int
  let of_float = of_float 
  let to_float = to_float
  let of_string = of_string 
  let to_string = to_string 
  let of_int32 = Int64.of_int32
  let to_int32 = Int64.to_int32
  let of_int64 x = x
  let to_int64 x = x
end

module Float = struct
  type t = float 
  let zero = 0.0
  let one = 1.0
  let (+:) = (+.)
  let (-:) = (-.)
  let ( *: ) = ( *. )
  let (/:) = ( /. )
  let (%:) = mod_float
  let abs = abs_float
  let (<<:) a b = failwith "Ops.Float: <<:"
  let (>>+) a b =  failwith "Ops.Float: >>+"
  let (>>:) a b =  failwith "Ops.Float: >>:"
  let (&:) a b =  failwith "Ops.Float: &:"
  let (|:) a b =  failwith "Ops.Float: |:"
  let (^:) a b =  failwith "Ops.Float: ^:"
  let (~:) a =  failwith "Ops.Float: ~:"
  let of_int = float_of_int
  let to_int = int_of_float
  let of_float x = x
  let to_float x = x
  let of_string = float_of_string
  let to_string = string_of_float
  let of_int32 = Int32.to_float
  let to_int32 = Int32.of_float
  let of_int64 = Int64.to_float
  let to_int64 = Int64.of_float
end



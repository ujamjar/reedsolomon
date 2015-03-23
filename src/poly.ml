(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* polynomial api.  powers are at array index positions *)

module type S = sig

  type elt
  type t

  val degree : t -> int
  val zero : t
  val one : t
  val x : t
  val to_poly : elt array -> t
  val of_poly : t -> elt array
  val copy : t -> t
  type poly_format = 
    {
      coef : elt -> string;
      indet : int -> string;
    }
  val poly_format : poly_format
  val string_format : bool -> poly_format -> int -> elt -> string
  val to_string : ?down:bool -> ?str:(int -> elt -> string) -> t -> string
  val trim : t -> t
  val slice : t -> int -> t

  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
  val ( *: ) : t -> t -> t
  val (/:) : t -> t -> t * t
  val ( *:. ) : t -> elt -> t
  val (/:.) : t -> elt -> t
  val (^:) : t -> int -> t
  val ( **: ) : t -> int -> t

  val ext_gcd : t -> t -> t * t

  val eval : t -> elt -> elt

end 

module Make(E : Ops.OpsBase) = struct

  type elt = E.t
  type t = elt array

  let string_of_elt = E.to_string

  let degree p = Array.length p - 1

  let zero = [| E.zero |]
  let one = [| E.one |]
  let x = [| E.zero;E.one |]

  (* remove high powers which are 0 *)
  let trim' p = 
    (* look for trailing zero's *)
    let deg = degree p in
    if deg < 0 then p 
    else
      let rec cnt n =
        if n = 0 then n
        else if p.(n) = E.zero then cnt (n-1)
        else n
      in
      let c = cnt deg in
      if c = deg then p
      else Array.init (c+1) (fun i -> p.(i))

  (* ensure '0' is [|0|] *)
  let trim p = 
    let p = trim' p in
    if degree p < 0 then zero
    else p

  let to_poly p = trim p
  let of_poly p = p

  let slice p deg = 
    let pdeg = degree p in
    if deg = pdeg then p
    else
      Array.init (deg+1) (fun i -> 
          if i <= pdeg then p.(i)
          else E.zero)

  let copy p = Array.copy p

  type poly_format = 
    {
      coef : elt -> string;
      indet : int -> string;
    }

  let poly_format = 
    {
      coef = string_of_elt;
      indet = 
        function
        | 0 -> ""
        | 1 -> "x"
        | _ as n -> "x^" ^ string_of_int n;
    }

  let string_format filter fmt pow cof = 
    let coef = fmt.coef cof in
    let indet = fmt.indet pow in
    if filter && cof=E.zero then ""
    else if filter && cof=E.one then 
      match pow with
      | 0 -> coef
      | _ -> indet
    else 
      match coef,indet with
      | "","" -> ""
      | coef,"" -> coef
      | "",indet -> indet
      | _ -> coef ^ "." ^ indet

  let to_string ?(down=true) ?(str=(string_format true poly_format)) p = 
    let open Printf in
    let sep a s = 
      if a="" then s else (if down then s ^ " + " ^ a else a ^ " + " ^ s)
    in
    let p =
      List.filter ((<>)"") (Array.to_list (Array.mapi str p))
    in
    List.fold_left sep "" p
  (*Array.fold_left sep "" (Array.mapi str p)*)

  (* add polys *)
  let add a b = 
    let max = max (degree a) (degree b) in
    let a,b = slice a max, slice b max in
    trim (Array.init (max+1) E.(fun i -> a.(i) +: b.(i)))

  let sub a b = 
    let max = max (degree a) (degree b) in
    let a,b = slice a max, slice b max in
    trim (Array.init (max+1) E.(fun i -> a.(i) -: b.(i)))

  (* raise to power of x *)
  let pow_x a b = 
    if b < 0 then 
      (try trim (Array.init (degree a + b + 1) (fun i -> a.(i-b)))
       with _ -> zero)
    else if b=0 then trim a
    else trim (Array.concat [ Array.make b E.zero; a ])

  (* scale poly *)
  let mul_scalar a b = trim (Array.map E.(fun a -> a *: b) a)
  let div_scalar a b = trim (Array.map E.(fun a -> a /: b) a)

  (* multiply polys *)
  let mul a b = 
    let a, b = trim a, trim b in
    let factors = Array.mapi (fun n a -> pow_x (mul_scalar b a) n) a in
    trim (Array.fold_left add [| E.zero |] factors)

  (* polynomial divison *)
  let div a b = 
    let a, b = trim a, trim b in
    if b = zero then failwith "poly divide by zero"
    else if b = one then a, zero
    else if degree b = 0 then div_scalar a b.(0), zero
    else
      let da = degree a in
      let db = degree b in
      let rec div a q = 
        let da = degree a in
        if da < db then q,a
        else
          let n = da-db in
          let s = E.(a.(da) /: b.(db)) in
          let a' = trim (slice (sub a (mul_scalar (pow_x b n) s)) (da-1)) in
          div a' ((n,s)::q)
      in
      let q,r = div a [] in
      trim (Array.init da (fun i -> try List.assoc i q with _ -> E.zero)), r

  let rec ext_gcd a b = 
    if b = zero then one, zero
    else 
      let q, r = div a b in
      let s, t = ext_gcd b r in
      t, sub s (mul q t)

  let eval p x = 
    let deg = degree p in
    let rec f n x' = 
      if n > deg then E.zero
      else 
        let a = E.(x' *: p.(n)) in
        E.(a +: f (n+1) (x' *: x))
    in
    E.(p.(0) +: f 1 x)

  let (+:) = add
  let (-:) = sub
  let ( *: ) = mul
  let (/:) = div
  let ( *:. ) = mul_scalar
  let (/:.) = div_scalar
  let (^:) = pow_x
  let rec ( **: ) x = function
    | 0 -> one
    | 1 -> x
    | _ as n -> x *: (x **: (n-1))

end


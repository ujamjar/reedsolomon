(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

type rsparams = {
  m : int; (* bits per symbol *)
  k : int; (* message symbols *)
  t : int; (* correction capability, 2t=parity symbols *)
  n : int; (* codeword symbols *)
  b : int; (* starting root of generator *)
  prim_poly : int; (* primitive polynomial *)
  prim_elt : int; (* primitive element *)
}

type rspoly = int array

let rspoly n = Array.make n 0

let to_string = 
  Array.fold_left 
    (fun s x -> s ^ " " ^ string_of_int x)
    "" 

type rscodec = {
  params : rsparams;
  encode : rspoly -> rspoly -> unit;
  decode : rspoly -> rspoly -> int;
}

let init p = 

  (* generate the reference implementation *)
  let module Gp = struct
    let pp = p.prim_poly
    let pe = p.prim_elt
  end in
  let module Rp = struct
    let t = p.t
    let k = p.k
    let b = p.b
  end in
  let module R = Codec.MakeStandard(Gp)(Rp) in
  let module G = R.G in

  (* array utilities *)
  let len = Array.length in
  let clear (a: int array) = for i=0 to len a - 1 do a.(i) <- 0 done in
  let get (a: int array) i = if i < 0 then G.zero else if i >= len a then G.zero else a.(i) in
  let iteri (a: int array) (f : int -> int) = for i=0 to len a - 1 do a.(i) <- f i done in
  let copy (t: int array) (f:int array) = for i=0 to len t - 1 do t.(i) <- f.(i) done in
  let shiftup (a: int array) (b: int array) = for i=len a - 1 downto 0 do a.(i) <- get a (i-1) done in
  let shiftdown (a: int array) (b: int array) = for i=0 to len a - 1 do a.(i) <- get b (i+1) done in

  let encode = 
    let t2 = 2 * p.t in
    let generator = Array.init t2 (fun i -> R.R.generator.(t2-i-1)) in
    (fun message parity -> 
       let k = len message in
       clear parity;
       for j=0 to k - 1 do
         let s = R.G.( message.(j) +: parity.(0) ) in
         for i=0 to t2 - 2 do
           parity.(i) <- R.G.((s *: generator.(i)) +: parity.(i+1))
         done;
         parity.(t2-1) <- R.G.(s *: generator.(t2-1))
       done
    )
  in

  let decode = 
    let t2 = 2 * p.t in

    (* roots of the generator polynomial *)
    let roots = Array.init t2 (fun i -> R.R.root i) in

    (* syndromes *)
    let syndromes = rspoly t2 in

    (* horners rule for polynomial evaluation *)
    let acc = ref G.zero in
    let horner a p = 
      acc := G.zero;
      for i=0 to Array.length p - 1 do
        acc := G.( (!acc *: a) +: p.(i) )
      done;
      !acc
    in

    let rhorner a p = 
      acc := G.zero;
      for i=Array.length p-1 downto 0 do
        acc := G.( (!acc *: a) +: p.(i) )
      done;
      !acc
    in

    (* inversion-less berlekamp-massy *)
    let riBM = 
      let t = p.t in
      let z,o = G.zero, G.one in
      let lambda = Array.make (t+1) z in
      let lambda' = Array.make (t+1) z in
      let b = Array.make (t+1) z in
      let delta = Array.make (2*t) z in
      let delta' = Array.make (2*t) z in
      let theta = Array.make (2*t) z in
      let gamma = ref o in
      let k = ref 0 in

      let init s = 
        let f i = if i=0 then o else z in
        iteri lambda f;
        iteri b f;
        copy delta s;
        copy theta s;
        gamma := o;
        k := 0;
      in

      let iter () = 
        let update_lambda i =
          G.((!gamma *: lambda.(i)) -: (delta'.(0) *: get b (i-1)))
        in
        let update_delta i = 
          G.((!gamma *: get delta' (i+1)) -: (delta'.(0) *: theta.(i)))
        in
        for r=0 to 2*t-1 do (* this is systolic, so much data is 0 during
                               part of the iteration *)
          (* step 1 *)
          copy lambda' lambda;
          copy delta' delta;
          iteri lambda update_lambda;
          iteri delta update_delta;
          (* step 2 *)
          if delta'.(0) <> z && !k >= 0 then begin
            copy b lambda'; (* previous lambda *)
            shiftdown theta delta';
            gamma := delta'.(0);
            k := - !k - 1
          end else begin
            shiftup b b;
            k := !k + 1
          end;
        done
      in

      (fun s d l ->
         init s;
         iter ();
         for i=0 to t-1 do d.(i) <- delta.(i) done;
         for i=0 to t do l.(i) <- lambda.(i) done)
    in

    let delta = rspoly p.t in
    let lambda = rspoly (p.t+1) in
    let lambda' = rspoly ((p.t+1)/2) in

    (* chien search *)
    let error_locs = rspoly p.t in
    let n_errors = ref 0 in
    let chien () =
      for n=0 to p.n-1 do
        if rhorner (G.antilog n) lambda = G.zero then begin
          error_locs.(!n_errors) <- n;
          incr n_errors;
        end
      done
    in

    let deriv () = 
      for i=0 to ((p.t+1)/2)-1 do
        lambda'.(i) <- lambda.((i*2)+1)
      done
    in

    (* forney *)
    let error_magnitudes = rspoly p.t in
    let forney () = 
      for i=0 to !n_errors-1 do
         let x' = G.antilog error_locs.(i) in
         let x =  G.(x' **: (p.b+t2-1)) in
         let x2 = G.(x' **: 2) in
         error_magnitudes.(i) <- G.(x *: (rhorner x' delta /: rhorner x2 lambda'))
      done
    in

    let correct c = 
      for i=0 to !n_errors-1 do
        let loc = error_locs.(i) in
        let loc = if loc = 0 then p.n-1 else loc-1 in
        let mag = error_magnitudes.(i) in
        c.(loc) <- G.(c.(loc) +: mag)
      done
    in

    (*let dump a = 
      let open Printf in
      for i=0 to Array.length a - 1 do
        printf "%i " a.(i)
      done;
      printf "\n"
    in*)

    let ssum = ref G.zero in
    (fun received corrected -> 
      n_errors := 0;
      ssum := G.zero;
      for i=0 to t2-1 do
        syndromes.(i) <- horner roots.(i) received;
        ssum := !ssum lor syndromes.(i)
      done;
      for i=0 to p.n-1 do
        corrected.(i) <- received.(i)
      done;
      if !ssum <> 0 then begin
        riBM syndromes delta lambda;
        chien ();
        deriv ();
        forney ();
        correct corrected
      end;
      !n_errors)
  in

  { params=p; encode; decode }



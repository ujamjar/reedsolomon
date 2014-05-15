(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
(* NOT COMPLETE.
 *
 * This is intended to be a fast path version of the code
 * which might actually be useful *)
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

(* GF(2^m), n=2^m-1 (may be shortened), k=n-2t *)

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

let rspoly n = Array.create n 0

let to_string = 
    Array.fold_left 
        (fun s x -> s ^ " " ^ string_of_int x)
        "" 

type rscodec = {
    params : rsparams;
    encode : rspoly -> rspoly -> unit;
    decode : rspoly -> rspoly -> unit;
}

(************************************************************)
(* test stuff *)
let bbc = 
    {
        m = 4;
        k = 11;
        t = 2;
        n = 15;
        b = 0;
        prim_poly = 19;
        prim_elt = 2;
    }

module Gp = struct
    let pp = 19
    let pe = 2
end 
module Rp = struct
    let t = 2
    let k = 11
    let b = 0
end 
module R = Rs.MakeStandard(Gp)(Rp) 
(************************************************************)

let rscodec p = 

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
    let module R = Rs.MakeStandard(Gp)(Rp) in
    let module G = R.G in

    (* array utilities *)
    let len = Array.length in
    let clear a = for i=0 to len a - 1 do a.(i) <- 0 done in
    let get a i = if i < 0 then G.zero else if i >= len a then G.zero else a.(i) in
    let iteri a f = for i=0 to len a - 1 do a.(i) <- f i done in
    let copy t f = for i=0 to len t - 1 do t.(i) <- f.(i) done in
    let shiftup a b = for i=len a - 1 downto 0 do a.(i) <- get a (i-1) done in
    let shiftdown a b = for i=0 to len a - 1 do a.(i) <- get b (i+1) done in

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
        let horner a p = Array.fold_left G.(fun acc x -> (acc *: a) +: x) G.zero p in

        (* inversion-less berlekamp-massy *)
        let riBM = 
            let t = p.t in
            let z,o = G.zero, G.one in
            let lambda = Array.create (t+1) z in
            let lambda' = Array.create (t+1) z in
            let b = Array.create (t+1) z in
            let delta = Array.create (2*t) z in
            let delta' = Array.create (2*t) z in
            let theta = Array.create (2*t) z in
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

        (* chien search *)
        let error_locs = rspoly p.t in
        let chien lambda =
            let p = ref (p.n-1) in 
            while !p >= 0 do
                decr p;
            done
        in

        (* output of berlekamp-massey *)
        let delta = rspoly p.t in
        let lambda = rspoly (p.t+1) in

        (fun received corrected -> 

            (* calculate syndromes *)
            for i=0 to t2-1 do
                syndromes.(i) <- horner roots.(i) received
            done;

            (* berlekamp massey *)
            riBM syndromes delta lambda;
            
            (* copy syndromes to output for now *)
            for i=0 to p.t do
                corrected.(i) <- lambda.(i)
            done
        )  (* 9;4 - 1;14;14 *)
    in

    { params=p; encode=encode; decode=decode }


let rs = rscodec bbc
let data = [|1;2;3;4;5;6;7;8;9;10;11|]
let parity = rspoly (2*bbc.t)
let () = rs.encode data parity
let _ = parity

let codeword = Array.concat [data;parity]
let received = Array.mapi 
    R.G.(fun i x ->
        if i=5 then 13 +: x
        else if i=12 then 2 +: x
        else x) codeword

let syndromes = rspoly (2*bbc.t)
let () = rs.decode received syndromes
let _ = syndromes



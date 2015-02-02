(* 
 * reedsolomon - error correction CODEC
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: 
 *
 *)

module type RsParams = 
sig
    val k : int
    val t : int
    val b : int
end

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
    val decode_errors_and_erasures_berlekamp_massey : poly -> int list -> poly
    val decode_errors_and_erasures : poly -> int list -> poly
end

module MakePoly(G : Galois.Table.Ops)(P : RsParams) = struct

    type elt = G.t

    module M = Matrix.Make(G)
    module R = Poly.Make(G)

    type poly = R.t
    type loc = int

    let root i = G.(alpha **: (P.b + i))

    let generator = 
        let roots = 
            Array.init (2*P.t)
                (fun i -> [| root i; G.one |])
        in
        List.fold_left (fun p' p -> R.(p' *: p)) R.one (Array.to_list roots)

    let xn n = R.(one ^: n)

    let x2t = xn (2 * P.t)

    let parity d = R.(snd ((d *: x2t) /: generator))

    let encode d = R.(trim ((d *: x2t) +: parity d))

    let horner p a = 
        let p = List.rev (Array.to_list p) in
        List.fold_left G.(fun acc x -> (acc *: a) +: x) G.zero p

    let syndromes r = Array.init (2*P.t) (fun i -> horner r (root i))

    let key_equations s v = 
        M.init v v (fun r c -> s.(v-(c+1)+r)),
        M.init v 1 (fun r c -> s.(v+r))

    (* K.L = S => K^-1.S = L, iff det(S) <> 0 *)
    let solve_key_equations (k,s) = 
        let k' = M.gauss_jordan_inverse k in
        M.(k' *: s)

    let peterson s = 
        let rec p v =
            if v = 0 then R.zero
            else
                let km, kv = key_equations s v in
                (* rather than check the determinant (which is VERY slow), 
                 * invert the matrix, then check it actually worked *)
                let km' = M.gauss_jordan_inverse km in
                if M.(km' *: km = identity v) then
                    let v = M.(km' *: kv) in
                    Array.init (M.rows v + 1) (fun i -> 
                        if i=0 then G.one else v.(i-1).(0))
                else
                    p (v-1)
        in
        p P.t

    let euclid_inner (r0,r1) (s0,s1) = 
        let q,r = R.(r1 /: r0) in
        let s = R.(s1 -: (q *: s0)) in
        r, s

    (* calculate error locator and value polys using extended gcd *)
    let euclid ?(norm=false) ?(lim=P.t) s = 
        let open R in
        let rec f (r0,r1) (s0,s1) = 
            if degree r0 < lim then r0,s0
            else
                let r, s = euclid_inner (r0,r1) (s0,s1) in
                f (r,r0) (s,s0) 
        in
        let v, l = f (R.trim s,x2t) (one,zero) in
        if norm then
            let inv' = G.( one /: l.(0) ) in
            Array.map G.(( *: ) inv') v,
            Array.map G.(( *: ) inv') l 
        else
            v, l

    let berlekamp_massey_iter s k (n,c,l) = 
        let get a n = if n >= Array.length a then G.zero else a.(n) in
        let rec e i v = 
            if i>l then v else e (i+1) G.(v +: (get n i *: get s (k-1-i))) 
        in
        let e = e 1 s.(k-1) in
        let n, c, l = 
            if e = G.zero then 
                n,c,l
            else
                let n' = R.(n +: (c *:. e)) in
                let c, l = 
                    if 2*l < k then R.(n /:. e), k-l else c, l
                in
                n', c, l
        in
        n, R.(c ^: 1), l

    (* calculate error locator poly using the berlekamp-massey algorithm *)
    let berlekamp_massey s = 
        let one, x = R.(one, one ^: 1) in
        let rec f k (n,c,l as x) = 
            if k > 2*P.t then n
            else 
                let x = berlekamp_massey_iter s k x in
                f (k+1) x
        in
        f 1 (one, x, 0)

    let deriv l = 
        (* set even powers to 0 *)
        let l = Array.init (Array.length l) (fun i -> 
            if i mod 2 = 0 then G.zero else l.(i)) in
        (* divide by x *)
        Array.init (Array.length l - 1) (fun i -> l.(i+1))

    module Sarwate = struct

        let t = P.t

        let get a i = 
            if i < 0 then G.zero
            else if i >= Array.length a then G.zero 
            else a.(i)
        let foldi f z n = 
            let acc = ref z in
            for i=0 to n-1 do
                acc := f !acc i
            done;
            !acc
        let iteri a f = 
            for i=0 to Array.length a - 1 do
                a.(i) <- f i
            done
        let copy t f = 
            for i=0 to Array.length t - 1 do
                t.(i) <- f.(i)
            done
        let shiftup a b = 
            for i=Array.length a - 1 downto 0 do
                a.(i) <- get a (i-1)
            done
        let shiftdown a b = 
            for i=0 to Array.length a - 1 do
                a.(i) <- get b (i+1)
            done

        let iBM = 
            let z,o = G.zero, G.one in
            let lambda = Array.make (t+1) z in
            let lambda' = Array.make (t+1) z in
            let b = Array.make (t+1) z in
            let k = ref 0 in
            let gamma = ref o in
            let delta = ref z in

            let init () = 
                let f i = if i=0 then o else z in
                iteri lambda f;
                iteri b f;
                k := 0;
                gamma := o;
                delta := z;        
            in

            let iter s = 
                let update_delta r d i = G.(d +: (get s (r-i) *: lambda.(i))) in
                let update_lambda i = 
                    G.((!gamma *: lambda.(i)) -: (!delta *: get b (i-1)))
                in
                for r=0 to 2*t-1 do
                    (* step 1 *)
                    delta := foldi (update_delta r) z (t+1);
                    (* step 2 *)
                    copy lambda' lambda;
                    iteri lambda update_lambda;
                    (* step 3 *)
                    if !delta <> z && !k >= 0 then begin
                        copy b lambda'; (* previous lambda *)
                        gamma := !delta;
                        k := - !k - 1
                    end else begin
                        shiftup b b;
                        gamma := !gamma;
                        k := !k + 1
                    end;
                done
                (* XXX step 4 *)
            in

            (fun s ->
                init ();
                iter s;
                Array.init (t+1) (fun i -> lambda.(i)))

        let riBM = 
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
                for r=0 to 2*t-1 do
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
                        (*copy theta theta;
                        gamma := !gamma;*)
                        k := !k + 1
                    end;
                done
            in

            (fun s ->
                init s;
                iter ();
                Array.init t (fun i -> delta.(i)),
                Array.init (t+1) (fun i -> lambda.(i)))

        let rriBM = 
            let z,o = G.zero, G.one in
            let delta = Array.make (3*t+1) z in
            let delta' = Array.make (3*t+1) z in
            let theta = Array.make (3*t+1) z in
            let gamma = ref o in
            let k = ref 0 in

            let init s = 
                let f i = 
                    if i<2*t then s.(i)
                    else if i=3*t then o
                    else z
                in
                iteri delta f;
                copy theta delta;
                gamma := o;
                k := 0;
            in

            let iter () =
                let update_delta i = 
                    G.((!gamma *: get delta' (i+1)) -: (delta'.(0) *: theta.(i)))
                in
                for i=0 to 2*t-1 do
                    (* step 1 *)
                    copy delta' delta;
                    iteri delta update_delta;
                    (* step 2 *)
                    if delta'.(0) <> z && !k >= 0 then begin
                        shiftdown theta delta';
                        gamma := delta'.(0);
                        k := - !k - 1
                    end else begin
                        (*copy theta theta;
                        gamma := !gamma;*)
                        k := !k + 1
                    end;
                done
        in

        (fun s ->
            init s;
            iter ();
            Array.init t (fun i -> delta.(i)),
            Array.init (t+1) (fun i -> delta.(t+i)))

        let forney v l = 
            let l' = deriv l in
            (fun x' ->
                let x' = G.antilog x' in
                let x =  G.(x' **: (P.b+(2*P.t)-1)) in
                G.(x *: (horner v x' /: horner l' x')))

    end

    let chien l = 
        let rec f n = 
            if n = (G.n_elems-1) then []
            else
                if horner l (G.antilog n) = G.zero then
                    n :: f (n+1)
                else
                    f (n+1)
        in
        f 0

    let error_location n = G.(log (inv (antilog n)))

    let error_magnitude v l s =
        let get a n = if n >= Array.length a then G.zero else a.(n) in
        Array.init v (fun i -> 
            let a = Array.init (i+1) (fun j -> G.(get s j *: get l (i-j))) in
            Array.fold_left G.(+:) G.zero a)

    let forney v l = 
        let l' = deriv l in
        (fun x' ->
            let x' = G.antilog x' in
            let x =  G.(x' **: (P.b-1)) in
            G.(x *: (horner v x' /: horner l' x')))

    let error v l = 
        let x = List.map2 (fun a b -> error_location a,b) l v in
        let n = P.k + P.t*2 in
        R.to_poly (Array.init n (fun i -> try List.assoc i x with _ -> G.zero))
        
    let correct r e = R.(r +: e)

    (* error correction *)

    let decode_euclid r = 
        let s = syndromes r in
        if Array.fold_left (fun b s -> b && s = G.zero) true s then r
        else
            let v, l = euclid s in
            let el = chien l in
            let ev = List.map (forney v l) el in
            let e = error ev el in
            correct r e

    let decode_berlekamp_massey r = 
        let s = syndromes r in
        if Array.fold_left (fun b s -> b && s = G.zero) true s then r
        else
            let l = berlekamp_massey s in
            let el = chien l in
            let v = error_magnitude (List.length el) l s in
            let ev = List.map (forney v l) el in
            let e = error ev el in
            correct r e

    let decode_peterson r = 
        let s = syndromes r in
        if Array.fold_left (fun b s -> b && s = G.zero) true s then r
        else
            let l = peterson s in
            let el = chien l in
            let v = error_magnitude (List.length el) l s in
            let ev = List.map (forney v l) el in
            let e = error ev el in
            correct r e

    let decode = decode_euclid

    (* erasure correction *)

    let erasure_locator y = 
        let terms = List.map (fun y -> [| G.one; G.(alpha **: y) |]) y in
        List.fold_left (fun a x -> R.(a *: x)) R.one terms

    let zero_erasures r y = 
        let r = Array.copy r in
        List.iter (fun y -> r.(y) <- G.zero) y;
        r

    let error_and_erasure ev el fv fl = 
        let e = List.map2 (fun a b -> error_location a,b) el ev in
        let f = List.map2 (fun a b -> a,b) fl fv in
        let x = e @ f in
        let n = P.k + P.t*2 in
        R.to_poly (Array.init n (fun i -> try List.assoc i x with _ -> G.zero))


    let decode_erasures_euclid r y = 
        if y = [] then r 
        else
            let f = List.length y in
            let tau = erasure_locator y in
            let r = zero_erasures r y in
            let s = syndromes r in
            let xi = R.(slice (tau *: s) (2*P.t-1)) in
            let omega, lambda = euclid ~norm:true ~lim:(P.t + (f / 2) + 0) xi in
            let phi = R.(tau *: lambda) in
            let forney = forney omega phi in
            let fv = List.map forney 
                (List.map (fun y -> G.(log (inv (alpha **: y)))) y)
            in
            let e = error_and_erasure [] [] fv y in
            correct r e

    let decode_erasures = decode_erasures_euclid

    let decode_errors_and_erasures_euclid r y = 
        let f = List.length y in
        let tau = erasure_locator y in
        let r = zero_erasures r y in
        let s = syndromes r in
        let xi = R.(slice (tau *: s) (2*P.t-1)) in
        let omega, lambda = euclid ~norm:true ~lim:(P.t + (f / 2) + 0) xi in
        let el = chien lambda in
        let phi = R.(tau *: lambda) in
        let forney = forney omega phi in
        let ev = List.map forney el in
        let fv = List.map forney 
            (List.map (fun y -> G.(log (inv (alpha **: y)))) y)
        in
        let e = error_and_erasure ev el fv y in
        correct r e

    let decode_errors_and_erasures_berlekamp_massey r y = 
        (*let f = List.length y in*)
        let tau = erasure_locator y in
        let r = zero_erasures r y in
        let s = syndromes r in
        let xi = R.(slice (tau *: s) (2*P.t-1)) in
        (*let omega, lambda = euclid ~norm:true ~lim:(P.t + (f / 2) + 0) xi in*)

        let lambda = berlekamp_massey xi in
        let omega = R.(slice (lambda *: xi) (2*P.t-1)) in

        let el = chien lambda in
        let phi = R.(tau *: lambda) in
        let forney = forney omega phi in
        let ev = List.map forney el in
        let fv = List.map forney 
            (List.map (fun y -> G.(log (inv (alpha **: y)))) y)
        in
        let e = error_and_erasure ev el fv y in
        correct r e

    let decode_errors_and_erasures = decode_errors_and_erasures_euclid

end

module type Standard = sig
    module Gp : Galois.Table.Params
    module G : Galois.Table.Ops with type t = int
    module Rp : RsParams
    module R : RsPoly with type elt = int
end

module MakeStandard(Gp : Galois.Table.Params)(Rp : RsParams) = struct
    module Gp = Gp
    module G = Galois.Table.MakeInt(Gp)
    module Rp = Rp
    module R = MakePoly(G)(Rp)
end

module BBCTest = MakeStandard
    (struct
        let pp = 19
        let pe = 2
    end)
    (struct
        let t = 2
        let k = 15 - (2*t)
        let b = 0
    end)

module CCSDS = struct

    let dual_of_poly = [|
        0x00;0x7b;0xaf;0xd4;0x99;0xe2;0x36;0x4d;
        0xfa;0x81;0x55;0x2e;0x63;0x18;0xcc;0xb7;
        0x86;0xfd;0x29;0x52;0x1f;0x64;0xb0;0xcb;
        0x7c;0x07;0xd3;0xa8;0xe5;0x9e;0x4a;0x31;
        0xec;0x97;0x43;0x38;0x75;0x0e;0xda;0xa1;
        0x16;0x6d;0xb9;0xc2;0x8f;0xf4;0x20;0x5b;
        0x6a;0x11;0xc5;0xbe;0xf3;0x88;0x5c;0x27;
        0x90;0xeb;0x3f;0x44;0x09;0x72;0xa6;0xdd;
        0xef;0x94;0x40;0x3b;0x76;0x0d;0xd9;0xa2;
        0x15;0x6e;0xba;0xc1;0x8c;0xf7;0x23;0x58;
        0x69;0x12;0xc6;0xbd;0xf0;0x8b;0x5f;0x24;
        0x93;0xe8;0x3c;0x47;0x0a;0x71;0xa5;0xde;
        0x03;0x78;0xac;0xd7;0x9a;0xe1;0x35;0x4e;
        0xf9;0x82;0x56;0x2d;0x60;0x1b;0xcf;0xb4;
        0x85;0xfe;0x2a;0x51;0x1c;0x67;0xb3;0xc8;
        0x7f;0x04;0xd0;0xab;0xe6;0x9d;0x49;0x32;
        0x8d;0xf6;0x22;0x59;0x14;0x6f;0xbb;0xc0;
        0x77;0x0c;0xd8;0xa3;0xee;0x95;0x41;0x3a;
        0x0b;0x70;0xa4;0xdf;0x92;0xe9;0x3d;0x46;
        0xf1;0x8a;0x5e;0x25;0x68;0x13;0xc7;0xbc;
        0x61;0x1a;0xce;0xb5;0xf8;0x83;0x57;0x2c;
        0x9b;0xe0;0x34;0x4f;0x02;0x79;0xad;0xd6;
        0xe7;0x9c;0x48;0x33;0x7e;0x05;0xd1;0xaa;
        0x1d;0x66;0xb2;0xc9;0x84;0xff;0x2b;0x50;
        0x62;0x19;0xcd;0xb6;0xfb;0x80;0x54;0x2f;
        0x98;0xe3;0x37;0x4c;0x01;0x7a;0xae;0xd5;
        0xe4;0x9f;0x4b;0x30;0x7d;0x06;0xd2;0xa9;
        0x1e;0x65;0xb1;0xca;0x87;0xfc;0x28;0x53;
        0x8e;0xf5;0x21;0x5a;0x17;0x6c;0xb8;0xc3;
        0x74;0x0f;0xdb;0xa0;0xed;0x96;0x42;0x39;
        0x08;0x73;0xa7;0xdc;0x91;0xea;0x3e;0x45;
        0xf2;0x89;0x5d;0x26;0x6b;0x10;0xc4;0xbf;
    |]

    let poly_of_dual = [|
        0x00;0xcc;0xac;0x60;0x79;0xb5;0xd5;0x19;
        0xf0;0x3c;0x5c;0x90;0x89;0x45;0x25;0xe9;
        0xfd;0x31;0x51;0x9d;0x84;0x48;0x28;0xe4;
        0x0d;0xc1;0xa1;0x6d;0x74;0xb8;0xd8;0x14;
        0x2e;0xe2;0x82;0x4e;0x57;0x9b;0xfb;0x37;
        0xde;0x12;0x72;0xbe;0xa7;0x6b;0x0b;0xc7;
        0xd3;0x1f;0x7f;0xb3;0xaa;0x66;0x06;0xca;
        0x23;0xef;0x8f;0x43;0x5a;0x96;0xf6;0x3a;
        0x42;0x8e;0xee;0x22;0x3b;0xf7;0x97;0x5b;
        0xb2;0x7e;0x1e;0xd2;0xcb;0x07;0x67;0xab;
        0xbf;0x73;0x13;0xdf;0xc6;0x0a;0x6a;0xa6;
        0x4f;0x83;0xe3;0x2f;0x36;0xfa;0x9a;0x56;
        0x6c;0xa0;0xc0;0x0c;0x15;0xd9;0xb9;0x75;
        0x9c;0x50;0x30;0xfc;0xe5;0x29;0x49;0x85;
        0x91;0x5d;0x3d;0xf1;0xe8;0x24;0x44;0x88;
        0x61;0xad;0xcd;0x01;0x18;0xd4;0xb4;0x78;
        0xc5;0x09;0x69;0xa5;0xbc;0x70;0x10;0xdc;
        0x35;0xf9;0x99;0x55;0x4c;0x80;0xe0;0x2c;
        0x38;0xf4;0x94;0x58;0x41;0x8d;0xed;0x21;
        0xc8;0x04;0x64;0xa8;0xb1;0x7d;0x1d;0xd1;
        0xeb;0x27;0x47;0x8b;0x92;0x5e;0x3e;0xf2;
        0x1b;0xd7;0xb7;0x7b;0x62;0xae;0xce;0x02;
        0x16;0xda;0xba;0x76;0x6f;0xa3;0xc3;0x0f;
        0xe6;0x2a;0x4a;0x86;0x9f;0x53;0x33;0xff;
        0x87;0x4b;0x2b;0xe7;0xfe;0x32;0x52;0x9e;
        0x77;0xbb;0xdb;0x17;0x0e;0xc2;0xa2;0x6e;
        0x7a;0xb6;0xd6;0x1a;0x03;0xcf;0xaf;0x63;
        0x8a;0x46;0x26;0xea;0xf3;0x3f;0x5f;0x93;
        0xa9;0x65;0x05;0xc9;0xd0;0x1c;0x7c;0xb0;
        0x59;0x95;0xf5;0x39;0x20;0xec;0x8c;0x40;
        0x54;0x98;0xf8;0x34;0x2d;0xe1;0x81;0x4d;
        0xa4;0x68;0x08;0xc4;0xdd;0x11;0x71;0xbd;  
    |] 

    module Gp = struct 
        let pp = 391
        let pe = 173
    end

    module Rs16' = MakeStandard(Gp)(struct
        let t = 16
        let k = 255-(2*t)
        let b = 128-t
    end)

    module Rs8' = MakeStandard(Gp)(struct
        let t = 8
        let k = 255-(2*t)
        let b = 128-t
    end)

    let dop = Array.map (Array.get dual_of_poly)
    let pod = Array.map (Array.get poly_of_dual)
    let (>>) f g x = g (f x)

    module Map(S : Standard) = struct
        module Gp = S.Gp
        module G = S.G
        module Rp = S.Rp
        module R = struct
            include S.R
            let parity = pod >> parity >> dop
            let encode = pod >> encode >> dop
            let decode = pod >> decode >> dop
        end
    end

    module Rs16 = Map(Rs16')
    module Rs8 = Map(Rs8')

end

module DVB = MakeStandard
    (struct
        let pp = 285
        let pe = 2
    end)
    (struct
        let t = 8
        let k = 188
        let b = 0
    end) 

module ATSC = MakeStandard
    (struct 
        let pp = 285
        let pe = 2
    end)
    (struct
        let t = 10
        let k = 187
        let b = 0
    end)

module G709 = MakeStandard
    (struct 
        let pp = 285
        let pe = 2
    end)
    (struct
        let t = 8
        let k = 239
        let b = 0
    end)


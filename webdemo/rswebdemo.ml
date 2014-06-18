(* Reed-Solomon tutorial *)
(*open Html_utils*)
(*open UJamJar*)
open HardCaml
open Rs
module D = Dom_html
module B = Bits.Comb.IntbitsList

(***********************************************************************)

let debug = true
let jlog s = if debug then Firebug.console##log(s)
let log s = if debug then jlog (Js.string s)

let ostr = Js.to_string
let jstr = Js.string
let jstri x = jstr (string_of_int x)

let get_element e = 
    let d = D.document in
    Js.Opt.get 
        (d##getElementById (Js.string ("jsoo_" ^ e)))
        (fun () -> 
          log ("get_element failed: " ^ e);
          assert false)

let rec delete_kids par = 
    let kid = par##firstChild in
    match Js.Opt.to_option kid with
    | Some(kid) ->  begin
        Dom.removeChild par kid;
        delete_kids par
    end
    | None -> 
        ()

let get_input n =
    match D.tagged (get_element n) with
    | D.Input(x) -> x
    | _ -> failwith ("couldn't find text element" ^ n)

let mk_table border n_rows n_cols f =
    let d = D.document in
    let table = D.createTable d in
    let tbody = D.createTbody d in
    Dom.appendChild table tbody;
    for row=0 to n_rows-1 do
        let trow = D.createTr d in
        for col=0 to n_cols-1 do
            let telt = D.createTd d in
            if (border row col) then begin
                let style = telt##style in
                style##borderStyle <- Js.string "solid";
                style##borderWidth <- Js.string "1px";
                style##borderColor <- Js.string "#d0d0d0";
            end;
            Dom.appendChild telt (f row col);
            Dom.appendChild trow telt
        done;
        Dom.appendChild tbody trow
    done;
    table

let mk_p ?(className="js_para") s = 
    let d = D.document in
    let t = D.createP d in
    t##className <- Js.string className;
    t##innerHTML <- Js.string s;
    t

(* XXX this is a bit yukky...
 * we invoke the 'old' handler then the new one to allow us to
 * chain them, however, the first time round there is no handler
 * so what happens?  We catch problems with exceptions.
 * I dont know how to detect when its empty - the old way
 * of checking against 'no_handler' worked in chrome but not firefox *)
let install_window_onload h = 
    let o = D.window##onload in
    D.window##onload <- D.handler (fun e ->
        let safe_invoke h =
            try ignore(D.invoke_handler h D.window e)
            with _ -> log "safe_invoke failed"
        in
        safe_invoke o;
        safe_invoke h;
        Js._false
    )

(***********************************************************************)

class type jq_math = object('self)
    method parseMath : Dom_html.element Js.t -> unit Js.meth
end
let math : jq_math Js.t = (Js.Unsafe.variable "window")##_M


(***********************************************************************)

module type Params = sig
    val m : int
    val n : int
    val k : int
    val t : int
    val b : int
    val pp : int
    val pe : int
    val decimal : bool
    val ud : bool
end

let value name = (get_input name)##value 
let int_value name = int_of_string (ostr (value name)) 

let get_m () = int_value "param_m"
let get_t () = int_value "param_t"
let get_pp () = int_value "param_pp"
let get_pe () = int_value "param_pe"
let get_b () = int_value "param_b"

let get_n () = (1 lsl (get_m())) - 1
let get_k () = get_n() - (get_t() * 2)

let get_dec () = Js.to_bool (get_input "show_decimal")##checked
let get_ud () = Js.to_bool (get_input "up_down")##checked

let set_int_value name x = (get_input name)##value <- jstr (string_of_int x)

let set_pp = set_int_value "param_pp"
let set_pe = set_int_value "param_pe"

let read_params () = 
    get_m(), get_t(), get_pp(), get_pe(), get_b(), get_dec(), get_ud()

let mk_params () = 
    let m,t,pp,pe,b,dec,ud = read_params () in
    let module P = struct
        let m = m
        let n = (1 lsl m) - 1
        let k = n - (2*t)
        let t = t
        let b = b
        let pp = pp
        let pe = pe
        let decimal = dec
        let ud = ud
    end in
    (module P : Params)

(***********************************************************************)

module Gen(P : Params) = struct

    let prim_poly = Array.of_list (List.rev (B.consti (Utils.nbits P.pp) P.pp)) 
    let prim_elt = Array.of_list (List.rev (B.consti (Utils.nbits P.pe) P.pe)) 

    module G' = Galois.Table.Make(struct
        module Ops = Galois.Extension.Make(struct
            module Poly = Poly.Make(Galois.Primitive.GF2)
            let pp = prim_poly
        end)
        let alpha = prim_elt
    end)

    let intg n = B.to_int (List.rev (Array.to_list n))
    
    module G = struct
        include G'
        let to_string x = string_of_int (intg x)
    end
    
    let constg n = G.(zero +: (Array.of_list (List.rev (B.consti P.m n))))

    module Rs = Rs.MakePoly(G)(P)

    (***********************************************************************)

    let sep s l = 
        List.fold_left (fun a x -> if a = "" then x else a ^ s ^ x) "" l

    let poly_split = 8

    let alpha = "α"
    let loc = "Λ"
    let omega = "Ω"

    let to_pow_string x = 
        if x = G.zero then "0"
        else
            alpha ^ "^" ^ string_of_int (G.log x)

    let pow_str v x n = (* v.x^n *)
        match n with
        | 0 -> v
        | 1 -> if v="1" then x else v ^ x
        | _ -> (if v="1" then "" else v) ^ x ^ "^" ^ string_of_int n

    let jqmath_of_poly' ?(var="x") zero to_string p = 
        let p = Array.to_list p in
        let _,p = List.fold_left (fun (p,l) x -> (p+1,(p,x)::l)) (0,[]) p in
        let p = List.filter (fun (_,v) -> v <> zero) p in
        let p = if P.ud then p else List.rev p in
        if p=[] then to_string zero
        else
            let rec split i l a = 
                if i=0 then List.rev a, l
                else
                    match l with
                    | [] -> split 0 l a
                    | h::t -> split (i-1) t (h::a)
            in
            let rec group l = 
                let h,t = split poly_split l [] in
                match t with
                | [] -> if h=[] then [] else [h]
                | _ -> h :: group t
            in
            let p = group p in
            let f p = 
                sep " + "
                    (List.map (fun (p,v) -> pow_str (to_string v) var p) p)
            in
            let p = List.map f p in
            let s = "\\table " ^ sep " + ;" p ^ ";" in
            log ("rendered into " ^ string_of_int (List.length p) ^ 
                " elements"); 
            if List.length p > 1 then "(" ^ s ^ ")"
            else s

    let jqmath_of_gf ?(var="x") p = jqmath_of_poly' ~var 0 string_of_int p
    let jqmath_of_poly ?(var="x") p = jqmath_of_poly' ~var G.zero G.to_string p
    let jqmath_of_poly_a ?(var="x") p = jqmath_of_poly' ~var G.zero to_pow_string p

    let jqmath_of_poly ?(var="x") p =
        if P.decimal then jqmath_of_poly ~var p
        else jqmath_of_poly_a ~var p

    let jqwrap s = "$$" ^ s ^ "$$"

    let jqmath_elt name str = 
        let div = get_element name in
        delete_kids div;
        Dom.appendChild div (mk_p (jqwrap str));
        math##parseMath(div)

    (***********************************************************************)

    let encoder d e = 
        let t = Rs.encode d in
        let r = Rs.R.(t +: e) in
        jqmath_elt "message_poly" (jqmath_of_poly d);
        jqmath_elt "generator_poly" (jqmath_of_poly Rs.generator);
        jqmath_elt "error_poly" (jqmath_of_poly e);
        jqmath_elt "codeword_poly" (jqmath_of_poly t);
        jqmath_elt "received_poly" (jqmath_of_poly r);
        t, r

    let string_of_syndrome s i = 
        "R(" ^ to_pow_string (Rs.root i) ^ "), =, " ^ to_pow_string s.(i) ^ 
                ", =, " ^ string_of_int (intg s.(i)) ^ ";"

    let euclid s = 
        let rec gen r s = 
            match r, s with
            | r0::r1::rt, s0::s1::st -> begin
                if Rs.R.degree r0 < P.t then r,s
                else
                    let r', s' = Rs.euclid_inner (r0,r1) (s0,s1) in
                    gen (r'::r) (s'::s)
            end
            | _ -> failwith "invalid"
        in
        let r, s = gen [Rs.R.trim s; Rs.x2t] [Rs.R.one; Rs.R.zero] in
        Array.of_list (List.rev r), Array.of_list (List.rev s)

    let gen_euclid_table s = 
        let r,s = euclid s in
        let x = Array.init (Array.length r*2) (fun i -> 
            if i mod 2 = 0 then r.(i/2) else s.(i/2))
        in
        let x = Array.mapi (fun i p ->
            (if i mod 2 = 0 then "r_{" else "s_{") ^ 
            string_of_int (i/2) ^ "}, =, " ^
            "{" ^ jqmath_of_poly p ^ "};") x
        in
        jqmath_elt "euclid" ("\\table " ^ Array.fold_left (^) "" x)

    let berlekamp_massey s = 
        let one, x = Rs.R.(one, one ^: 1) in
        let rec f k (n,c,l as x) = 
            if k > 2*P.t then []
            else 
                let x = Rs.berlekamp_massey_iter s k x in
                x :: f (k+1) x
        in
        f 1 (one, x, 0)

    let gen_berlekamp_massey_table s = 
        let x = berlekamp_massey s in
        let x = Array.of_list x in
        let x = Array.mapi (fun k (n,c,l) ->
            let k = string_of_int (k+1) in
            "L_{"^k^"}, =, " ^ string_of_int l ^ ";" ^
            "Λ_{"^k^"}, =, {" ^ jqmath_of_poly n ^ "};" ^ 
            "C_{"^k^"}, =, {" ^ jqmath_of_poly c ^ "};") x 
        in
        jqmath_elt "berlekamp_massey" ("\\table " ^ Array.fold_left (^) "" x)

    let decoder r = 
        (* syndromes *)
        let s = Rs.syndromes r in
        let s' = Array.init (2*P.t) (string_of_syndrome s) in
        jqmath_elt "syndromes" ("\\table " ^ Array.fold_left (^) "" s');
        jqmath_elt "syndrome_poly" (jqmath_of_poly s);
        (* euclid *)
        let v, l = Rs.euclid ~norm:true s in
        gen_euclid_table s;
        gen_berlekamp_massey_table s;
        jqmath_elt "error_locator_poly" (jqmath_of_poly l);
        jqmath_elt "error_magnitude_poly" (jqmath_of_poly v);
        (* chien *)
        let el = Rs.chien l in
        let el' = Array.init P.n (fun i ->
            let x = Rs.horner l (G.antilog i) in
            loc ^ "(" ^ to_pow_string (G.antilog i) ^ "), =, " ^ 
                string_of_int (intg x) ^ ", =, " ^ to_pow_string x ^ ";")
        in
        jqmath_elt "chien_search" ("\\table " ^ Array.fold_left (^) "" el');
        let el' = Array.of_list el in
        let el'' = Array.init (Array.length el') (fun i ->
            "X_{" ^ string_of_int (i+1) ^ "}^{-1}, =, " ^ 
                to_pow_string (G.antilog el'.(i)) ^ 
            ", thus, log_{α} X_{" ^ string_of_int (i+1) ^ "}, =, " ^ 
                string_of_int G.(log (inv (antilog el'.(i)))) ^ ";")
        in
        jqmath_elt "error_locations" ("\\table " ^ Array.fold_left (^) "" el'');
        (* forney *)
        let forney = Rs.forney v l in
        let ev = List.map forney el in
        let l' = Rs.deriv l in
        let f = "X_{j}^{"^ string_of_int (1-P.b) ^ "} {" ^ 
            jqmath_of_poly_a ~var:"X_{j}^{-1}" v ^ "} / {" ^ 
            jqmath_of_poly_a ~var:"X_{j}^{-1}" l' ^ "}"
        in
        jqmath_elt "forney_poly" f;
        let forney i el = 
            let x' = G.antilog el in
            let x = G.inv x' in
            let f = forney el in
            "Y_{" ^ string_of_int (i+1) ^ "}, =, " ^
            to_pow_string G.(x **: (1-P.b)) ^ 
                "{" ^ to_pow_string (Rs.horner v x') ^ " / " ^ 
                      to_pow_string (Rs.horner l' x') ^ "}, =, " ^ 
            to_pow_string f ^ ", =, " ^ string_of_int (intg f) ^ ";"
        in
        let f = Array.init (Array.length el') (fun i -> forney i el'.(i)) in
        jqmath_elt "forney_res" ("\\table " ^ Array.fold_left (^) "" f);
        (* correction *)
        let e' = Rs.error ev el in
        jqmath_elt "calc_error_poly" (jqmath_of_poly e');
        jqmath_elt "corrected_poly" (jqmath_of_poly Rs.R.(e'+:r))

end

(***********************************************************************)

let hide_galois_field _ = 
    (get_element "show_galois_field_div")##style##display <- jstr "none";
    Js._false

let toggle_galois_field _ = 
    let module X = Gen( (val (mk_params ()) : Params) ) in
    let open X in
    let box = get_element "show_galois_field_div" in
    if box##style##display = jstr "block" then begin
        hide_galois_field ()
    end else begin
        jqmath_elt "prim_poly" (jqmath_of_gf prim_poly);
        jqmath_elt "prim_elt" (jqmath_of_gf prim_elt);
        (* table of GF(2^m) elements *)
        let p = 
            Array.init G.n_elems (fun i ->
                let c = constg i in
                string_of_int i ^ ", {" ^ 
                    jqmath_of_gf c ^ "}, " ^ to_pow_string c ^ ";")
        in
        let s = "\\table " ^ Array.fold_left (^) "" p in
        jqmath_elt "galois_field" s;
        (get_element "show_galois_field_div")##style##display <- jstr "block";
        Js._false
    end

(***********************************************************************)

(* number input cell *)
let mk_num_input id = 
    let i = D.createInput ~_type:(jstr "text") D.document in
    i##id <-jstr id;
    i##maxLength <- 4;
    i##size <- 4;
    i##value <- jstr "0";
    i##onkeypress <- D.handler (fun e -> 
        (* only allow numbers *)
        let cc = e##charCode in
        match Js.Optdef.to_option cc with
        | None -> Js._true
        | Some(cc) -> 
            if cc > 31 && (cc < 48 || cc > 57) then Js._false
            else Js._true
    );
    i

type table_data = 
    {
        name : string;
        data : Dom_html.inputElement Js.t array ref;
    }

let message_data  = { name = "message_data"; data = ref [||]; }
let error_data  = { name = "error_data"; data = ref [||]; }

(* construct a new table, copy data from old one where possible *)
let mk_table_data n d = 
    if Array.length !(d.data) <> n then begin
        d.data := Array.init n 
            (fun i -> 
                let inp = mk_num_input (d.name ^ string_of_int i) in
                inp##value <- (try !(d.data).(i)##value with _ -> jstr "0");
                inp);
        let table = mk_table (fun _ _ -> false) ((n+7)/8) 8 (fun r c ->
            let i = (r*8)+c in
            try ((!(d.data).(i)) :> Dom.element Js.t)
            with _ -> (mk_p "" :> Dom.element Js.t))
        in
        let div = get_element d.name in
        delete_kids div;
        Dom.appendChild div table
    end

let set_table_data table data = 
    mk_table_data (Array.length data) table;
    Array.iteri (fun x i -> i##value <- jstr (string_of_int data.(x)))
        !(table.data)

let get_table_data table = 
    Array.map (fun i -> int_of_string (ostr (i##value))) !(table.data)

let read_data () = 
    get_table_data message_data, get_table_data error_data

let init_table table fn = 
    let data = Array.init (Array.length !(table.data)) fn in
    set_table_data table data;
    Js._false

(***********************************************************************)

let run_rs _ = 
    let module X = Gen( (val (mk_params ()) : Params) ) in
    let d, e = read_data () in
    let t, r = X.encoder (Array.map X.constg d) (Array.map X.constg e) in
    X.decoder r;
    Js._false

let int_of_gf2_prim_poly m = 
    let x = Galois.GF2N.gf2_prim_polys.(m) in
    let x = B.to_int (List.rev (Array.to_list x)) in
    x

let derived_params _ = 
    let div = get_element "derived_params" in
    let p = mk_p
        ("n = " ^ string_of_int (get_n()) ^ 
            ", k = " ^ string_of_int (get_k ()))
    in
    delete_kids div;
    Dom.appendChild div p;
    Js._false

let update_tables _ = 
    let m, t, _,  _, _, _, _ = read_params () in
    let n = (1 lsl m) - 1 in
    let k = n - 2*t in
    mk_table_data k message_data;
    mk_table_data n error_data;
    Js._false

let select_poly _ = 
    set_pp (int_of_gf2_prim_poly (get_m ()));
    set_pe 2;
    Js._false

let init_table table fn _ = 
    let n,t = get_n(), get_t() in
    let fn = fn n t in
    init_table table fn

let onload _ = 
    log "loading reed-solomon tutorial app";

    (* example from the bbc white paper *)
    let d = [| 11;10;9;8;7;6;5;4;3;2;1 |] in
    let e = [| 0;0;2;0;0;0;0;0;0;13;0;0;0;0;0 |] in

    (* create input data tables *)
    set_table_data message_data d;
    set_table_data error_data e;

    (get_element "show_galois_field")##onclick <- 
        D.handler toggle_galois_field;

    (get_element "random_message")##onclick <- 
        D.handler (init_table message_data (fun n _ _ -> Random.int (n+1)));

    (get_element "random_errors")##onclick <- 
        D.handler (init_table error_data 
            (fun n t _ -> if Random.int n <= t then (Random.int n)+1 else 0));

    (get_element "clear_message")##onclick <- 
        D.handler (init_table message_data (fun _ _ _ -> 0));
    (get_element "clear_errors")##onclick <- 
        D.handler (init_table error_data (fun _ _ _ -> 0));

    let handlers l = (* run multiple handlers *)
        D.handler (fun e -> List.iter (fun f -> ignore (f e)) l; Js._false)
    in
    ignore (derived_params());

    (get_input "param_m")##onchange <- handlers
        [update_tables; hide_galois_field; select_poly; derived_params];
    (get_input "param_t")##onchange <- handlers 
        [update_tables; derived_params];

    (get_input "param_pp")##onchange <- D.handler hide_galois_field;
    (get_input "param_pe")##onchange <- D.handler hide_galois_field;

    (get_element "calculate_rs")##onclick <- D.handler run_rs;

    ignore (run_rs());

    Js._false

let _ = install_window_onload (D.handler onload)


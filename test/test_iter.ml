open Reedsolomon.Iter

module Test(X : sig val rsp : rsparams end) = struct
  
  open X

  let () = Printf.printf "%i bits/symbol, %i symbols/codeword, t=%i\n"
    rsp.m rsp.n rsp.t

  let rand_sort a = 
    for i=(Array.length a - 1) downto 1 do
      let swap = Random.int (i+1) in
      let tmp = a.(i) in
      a.(i) <- a.(swap);
      a.(swap) <- tmp
    done

  (* random error vector with 'e' errors *)
  let error e = 
    let e = Array.init rsp.n (fun i -> if i < e then 1 + Random.int rsp.n else 0) in
    rand_sort e;
    e

  let random n = Array.init n (fun i -> Random.int (rsp.n+1))

  let rs = init rsp

  let print a = 
    let open Printf in
    for i=0 to Array.length a - 1 do
      printf "%.2x" a.(i);
    done;
    printf "\n"

  (* correctness tests *)
  let test1 n_errors = 
    let data = random rsp.k in
    let parity = rspoly (rsp.t*2) in
    let () = rs.encode data parity in
    let message = Array.concat [ data;parity ] in
    let err = error n_errors in
    let received = Array.init rsp.n (fun i -> message.(i) lxor err.(i)) in
    let corrected = rspoly rsp.n in
    let n_corrections = rs.decode received corrected in
    if corrected <> message || n_corrections <> n_errors then begin
      Printf.printf "\n\nERROR:\n\n";
      print data;
      print message;
      print err;
      print received;
      print corrected;
      Printf.printf "n_errors=%i n_corrections=%i\n" n_errors n_corrections;
    end;
    n_corrections, corrected = message

  let testn nn = 
    for ne=0 to rsp.t do
      for n=0 to nn-1 do
        Printf.printf "\r%i %i%!" ne n;
        let ce,ok = test1 ne in
        assert (ce=ne);
        assert ok;
      done;
      Printf.printf "\r                          \r%!";
    done;
    Printf.printf "OK\n%!"

  let () = testn 10000

  (* performance tests. *)
  let perf n_errors n_tests = 
    let data = random rsp.k in
    let parity = rspoly (rsp.t*2) in
    let () = rs.encode data parity in
    let message = Array.concat [ data;parity ] in
    let err = error n_errors in
    let received = Array.init rsp.n (fun i -> message.(i) lxor err.(i)) in
    let corrected = rspoly rsp.n in
    for i=0 to n_tests-1 do
      ignore (rs.decode received corrected)
    done

  let perfs n_tests = 
    for ne=0 to rsp.t do
      let time = Unix.gettimeofday() in
      perf ne n_tests;
      let time = Unix.gettimeofday() -. time in
      Printf.printf "%i errors/cw %f secs %f Mbits/sec\n%!"
        ne time
        (float_of_int (rsp.n * rsp.m * n_tests) /. (1000000. *. time))
    done

  let () = perfs 10000

end

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

let g709 = 
  {
    m = 8;
    k = 239;
    t = 8;
    n = 255;
    b = 0;
    prim_poly = 285;
    prim_elt = 2;
  }

let g16 = 
  {
    m = 8;
    k = 255-32;
    t = 16;
    n = 255;
    b = 0;
    prim_poly = 285;
    prim_elt = 2;
  }

(* 4 bits/symbol, t=2 *)
module A = Test(struct let rsp = bbc end)
(* 8 bits/symbol, t=8 *)
module B = Test(struct let rsp = g709 end)
(* 8 bits/symbol, t=16 *)
module C = Test(struct let rsp = g16 end)


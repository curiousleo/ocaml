(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pseudo-random number generator
   This is a lagged-Fibonacci F(55, 24, +) with a modified addition
   function to enhance the mixing of bits.
   If we use normal addition, the low-order bit fails tests 1 and 7
   of the Diehard test suite, and bits 1 and 2 also fail test 7.
   If we use multiplication as suggested by Marsaglia, it doesn't fare
   much better.
   By mixing the bits of one of the numbers before addition (XOR the
   5 high-order bits into the low-order bits), we get a generator that
   passes all the Diehard tests.
*)

(* TODO: Update to return just a single int64 *)
external random_seed: unit -> int array = "caml_sys_random_seed"

module State = struct

  (****************)
  (* Xoshiro256++ *)
  (****************)

  (*
  type t = {
    mutable s0 : int64;
    mutable s1 : int64;
    mutable s2 : int64;
    mutable s3 : int64;
  }

  let new_state () = {
    s0 = 0xd7ba58ac4f5f8ebaL;
    s1 = 0x3602218b79c90891L;
    s2 = 0x35377574a3001a9eL;
    s3 = 0xf302f188176dfd1cL;
  }

  let rotl x k =
    Int64.(logor (shift_left x k) (shift_right_logical x (64 - k)))

  let step s =
    let t = Int64.shift_left s.s1 17 in
    s.s2 <- Int64.logxor s.s2 s.s0;
    s.s3 <- Int64.logxor s.s3 s.s1;
    s.s1 <- Int64.logxor s.s1 s.s2;
    s.s0 <- Int64.logxor s.s0 s.s3;
    s.s2 <- Int64.logxor s.s2 t;
    s.s3 <- rotl s.s3 45

  let bits64 s =
    let result = Int64.(add (rotl (add s.s0 s.s3) 23) s.s0) in
    step s;
    result

  let bits32 s =
    let b64 = bits64 s in
    Int64.(to_int32 (logand b64 0xFFFFFFFFL))

  let mix s seed =
    s.s0 <- Int64.(add s.s0 (of_int seed));
    step s

  let full_init s seed =
    s.s0 <- 0xd7ba58ac4f5f8ebaL;
    s.s1 <- 0x3602218b79c90891L;
    s.s2 <- 0x35377574a3001a9eL;
    s.s3 <- 0xf302f188176dfd1cL;
    step s;
    for i = 0 to Array.length seed - 1 do
      mix s seed.(i)
    done;
    for _ = 0 to 16 do
      step s
    done

  let copy s = {
    s0 = s.s0;
    s1 = s.s1;
    s2 = s.s2;
    s3 = s.s3;
  }

  let assign t s =
    t.s0 <- s.s0;
    t.s1 <- s.s1;
    t.s2 <- s.s2;
    t.s3 <- s.s3
  *)

  (****************)
  (* PCG RXS-M-XS *)
  (****************)

  type t = int64 ref

  let new_state () = ref 0x4d595df4d0f33173L

  let step s =
    let newstate = Int64.(add (mul !s 6364136223846793005L) 1442695040888963407L) in
    s := newstate

  let xsh_rr state =
    (* uint32_t xorshifted = ((state >> 18u) ^ state) >> 27u; *)
    let xorshifted = Int64.(to_int32 (shift_right_logical (logxor (shift_right_logical state 18) state) 27))
    (* uint32_t rot = state >> 59u; *)
    and rot = Int64.(to_int (shift_right_logical state 59)) in
    (* return (xorshifted >> rot) | (xorshifted << ((-rot) & 31)); *)
    Int32.(logor (shift_right_logical xorshifted rot) (shift_left xorshifted (~-rot land 31)))

  let rxs_m_xs state =
    let rot = Int64.(to_int (add (shift_right_logical state 59) 5L)) in
    let xorshifted = Int64.(logxor (shift_right_logical state rot) state) in
    let word = Int64.mul xorshifted 0xAEF17502108EF2D9L in
    Int64.(logxor (shift_right_logical word 43) word)

  let bits32 s =
    let oldstate = !s in
    step s;
    xsh_rr oldstate

  let bits64 s =
    let oldstate = !s in
    step s;
    rxs_m_xs oldstate

  let mix s seed =
    s := Int64.(add !s (of_int seed));
    step s

  let init s seed =
    s := 0L;
    step s;
    mix s seed

  let full_init s seed =
    s := 0L;
    step s;
    for i = 0 to Array.length seed - 1 do
      mix s seed.(i)
    done

  let copy s = ref !s

  let assign t s = t := !s

  (* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
  let bits s =
    Int32.(to_int (logand (bits32 s) 0x3FFFFFFFl))

  (*****************************)

  let make seed =
    let result = new_state () in
    full_init result seed;
    result

  let make_self_init () = make (random_seed ())


  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound

  let rec int63aux s n =
    let max_int_32 = (1 lsl 30) + 0x3FFFFFFF in (* 0x7FFFFFFF *)
    let b64 = bits64 s in
    let (r, max_int) =
      if n <= max_int_32 then
        (* 31 random bits on both 64-bit OCaml and JavaScript. *)
        let bpos = Int64.(to_int (logand b64 0x7FFFFFFFL))
        in
          (bpos, max_int_32)
      else
        (* 62 random bits on 64-bit OCaml; unreachable on JavaScript. *)
        let bpos = Int64.(to_int (logand b64 0x3FFFFFFFFFFFFFFFL))
        in
          (bpos, max_int)
    in
    let v = r mod n in
    if r - v > max_int - n + 1 then int63aux s n else v

  let full_int s bound =
    if bound <= 0 then
      invalid_arg "Random.full_int"
    else if bound > 0x3FFFFFFF then
      int63aux s bound
    else
      intaux s bound


  let rec int32aux s n =
    let b1 = Int32.of_int (bits s) in
    let b2 = Int32.shift_left (Int32.of_int (bits s land 1)) 30 in
    let r = Int32.logor b1 b2 in
    let v = Int32.rem r n in
    if Int32.sub r v > Int32.add (Int32.sub Int32.max_int n) 1l
    then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound


  let rec int64aux s n =
    let b1 = Int64.of_int (bits s) in
    let b2 = Int64.shift_left (Int64.of_int (bits s)) 30 in
    let b3 = Int64.shift_left (Int64.of_int (bits s land 7)) 60 in
    let r = Int64.logor b1 (Int64.logor b2 b3) in
    let v = Int64.rem r n in
    if Int64.sub r v > Int64.add (Int64.sub Int64.max_int n) 1L
    then int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound


  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))


  (* Returns a float 0 <= x <= 1 with at most 60 bits of precision. *)
  let rawfloat s =
    let scale = 1073741824.0  (* 2^30 *)
    and r1 = Stdlib.float (bits s)
    and r2 = Stdlib.float (bits s)
    in (r1 /. scale +. r2) /. scale


  let float s bound = rawfloat s *. bound

  let bool s = (bits s land 1 = 0)

  (*
  let bits32 s =
    let b1 = Int32.(shift_right_logical (of_int (bits s)) 14) in  (* 16 bits *)
    let b2 = Int32.(shift_right_logical (of_int (bits s)) 14) in  (* 16 bits *)
    Int32.(logor b1 (shift_left b2 16))

  let bits64 s =
    let b1 = Int64.(shift_right_logical (of_int (bits s)) 9) in  (* 21 bits *)
    let b2 = Int64.(shift_right_logical (of_int (bits s)) 9) in  (* 21 bits *)
    let b3 = Int64.(shift_right_logical (of_int (bits s)) 8) in  (* 22 bits *)
    Int64.(logor b1 (logor (shift_left b2 21) (shift_left b3 42)))
  *)

  let nativebits =
    if Nativeint.size = 32
    then fun s -> Nativeint.of_int32 (bits32 s)
    else fun s -> Int64.to_nativeint (bits64 s)

end

let default = State.new_state ()
let bits () = State.bits default
let int bound = State.int default bound
let full_int bound = State.full_int default bound
let int32 bound = State.int32 default bound
let nativeint bound = State.nativeint default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default
let bits32 () = State.bits32 default
let bits64 () = State.bits64 default
let nativebits () = State.nativebits default

let full_init seed = State.full_init default seed
let init seed = State.init default seed
let self_init () = full_init (random_seed())

(* Manipulating the current state. *)

let get_state () = State.copy default
let set_state s = State.assign default s

(********************

(* Test functions.  Not included in the library.
   The [chisquare] function should be called with n > 10r.
   It returns a triple (low, actual, high).
   If low <= actual <= high, the [g] function passed the test,
   otherwise it failed.

  Some results:

init 27182818; chisquare int 100000 1000
init 27182818; chisquare int 100000 100
init 27182818; chisquare int 100000 5000
init 27182818; chisquare int 1000000 1000
init 27182818; chisquare int 100000 1024
init 299792643; chisquare int 100000 1024
init 14142136; chisquare int 100000 1024
init 27182818; init_diff 1024; chisquare diff 100000 1024
init 27182818; init_diff 100; chisquare diff 100000 100
init 27182818; init_diff2 1024; chisquare diff2 100000 1024
init 27182818; init_diff2 100; chisquare diff2 100000 100
init 14142136; init_diff2 100; chisquare diff2 100000 100
init 299792643; init_diff2 100; chisquare diff2 100000 100
- : float * float * float = (936.754446796632465, 997.5, 1063.24555320336754)
# - : float * float * float = (80., 89.7400000000052387, 120.)
# - : float * float * float = (4858.57864376269, 5045.5, 5141.42135623731)
# - : float * float * float =
(936.754446796632465, 944.805999999982305, 1063.24555320336754)
# - : float * float * float = (960., 1019.19744000000355, 1088.)
# - : float * float * float = (960., 1059.31776000000536, 1088.)
# - : float * float * float = (960., 1039.98463999999512, 1088.)
# - : float * float * float = (960., 1054.38207999999577, 1088.)
# - : float * float * float = (80., 90.096000000005, 120.)
# - : float * float * float = (960., 1076.78720000000612, 1088.)
# - : float * float * float = (80., 85.1760000000067521, 120.)
# - : float * float * float = (80., 85.2160000000003492, 120.)
# - : float * float * float = (80., 80.6220000000030268, 120.)

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then Stdlib.float v.(i0) *. Stdlib.float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1


let chisquare g n r =
  if n <= 10 * r then invalid_arg "chisquare";
  let f = Array.make r 0 in
  for i = 1 to n do
    let t = g r in
    f.(t) <- f.(t) + 1
  done;
  let t = sumsq f 0 r
  and r = Stdlib.float r
  and n = Stdlib.float n in
  let sr = 2.0 *. sqrt r in
  (r -. sr,   (r *. t /. n) -. n,   r +. sr)


(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref 0
let init_diff r = st := int r
let diff r =
  let x1 = !st
  and x2 = int r
  in
  st := x2;
  if x1 >= x2 then
    x1 - x2
  else
    r + x1 - x2


let st1 = ref 0
and st2 = ref 0


(* This is to test for quadratic dependencies between successive random
   numbers.
*)
let init_diff2 r = st1 := int r; st2 := int r
let diff2 r =
  let x1 = !st1
  and x2 = !st2
  and x3 = int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r


********************)

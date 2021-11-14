```ocaml
let () =
  while true do
    let r = Random.bits32 (* or Random_pcg.bits32 *) () in
    (* Printf.printf "%08lx\n" r *)
    output_binary_int stdout (Int32.to_int r)
  done
```

# Current

```console
$ \time --format='%e real %U user %S system (%P cpu)' _build/default/measure.exe 
bbc40825
8.72 real 8.71 user 0.00 system (99% cpu)
```

```console
$ generate.exe | RNG_test stdin32
RNG_test using PractRand version 0.93
RNG = RNG_stdin32, seed = 0xd3ad21b5
test set = normal, folding = standard (32 bit)

rng=RNG_stdin32, seed=0xd3ad21b5
length= 256 megabytes (2^28 bytes), time= 2.5 seconds
  no anomalies in 124 test result(s)

rng=RNG_stdin32, seed=0xd3ad21b5
length= 512 megabytes (2^29 bytes), time= 5.1 seconds
  no anomalies in 132 test result(s)

rng=RNG_stdin32, seed=0xd3ad21b5
length= 1 gigabyte (2^30 bytes), time= 10.2 seconds
  no anomalies in 141 test result(s)

rng=RNG_stdin32, seed=0xd3ad21b5
length= 2 gigabytes (2^31 bytes), time= 20.1 seconds
  no anomalies in 148 test result(s)

rng=RNG_stdin32, seed=0xd3ad21b5
length= 4 gigabytes (2^32 bytes), time= 39.6 seconds
  Test Name                         Raw       Processed     Evaluation
  BCFN(2+2,13-0,T)                  R= +15.2  p =  1.2e-7   very suspicious  
  BCFN(2+3,13-0,T)                  R=  +8.6  p =  4.0e-4   unusual          
  [Low8/32]BCFN(2+1,13-1,T)         R= +12.7  p =  2.5e-6   suspicious       
  ...and 153 test result(s) without anomalies

rng=RNG_stdin32, seed=0xd3ad21b5
length= 8 gigabytes (2^33 bytes), time= 79.2 seconds
  Test Name                         Raw       Processed     Evaluation
  BCFN(2+2,13-0,T)                  R= +26.1  p =  1.7e-13    FAIL           
  BCFN(2+3,13-0,T)                  R= +14.1  p =  4.3e-7   very suspicious  
  BCFN(2+4,13-0,T)                  R=  +8.9  p =  2.6e-4   unusual          
  [Low8/32]BCFN(2+1,13-0,T)         R= +25.2  p =  5.0e-13    FAIL           
  ...and 161 test result(s) without anomalies
```

# Proposal

```console
$ \time --format='%e real %U user %S system (%P cpu)' _build/default/measure.exe 
78b30533
4.73 real 4.73 user 0.00 system (100% cpu)
```

```
generate.exe | RNG_test stdin32
RNG_test using PractRand version 0.93
RNG = RNG_stdin32, seed = 0x191622aa
test set = normal, folding = standard (32 bit)

rng=RNG_stdin32, seed=0x191622aa
length= 256 megabytes (2^28 bytes), time= 2.1 seconds
  no anomalies in 124 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 512 megabytes (2^29 bytes), time= 4.4 seconds
  no anomalies in 132 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 1 gigabyte (2^30 bytes), time= 8.9 seconds
  no anomalies in 141 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 2 gigabytes (2^31 bytes), time= 17.5 seconds
  no anomalies in 148 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 4 gigabytes (2^32 bytes), time= 34.3 seconds
  Test Name                         Raw       Processed     Evaluation
  [Low8/32]DC6-9x1Bytes-1           R=  -4.5  p =1-3.5e-3   unusual          
  ...and 155 test result(s) without anomalies

rng=RNG_stdin32, seed=0x191622aa
length= 8 gigabytes (2^33 bytes), time= 68.7 seconds
  no anomalies in 165 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 16 gigabytes (2^34 bytes), time= 137 seconds
  Test Name                         Raw       Processed     Evaluation
  [Low8/32]Gap-16:B                 R=  -4.2  p =1-1.6e-3   unusual          
  ...and 171 test result(s) without anomalies

rng=RNG_stdin32, seed=0x191622aa
length= 32 gigabytes (2^35 bytes), time= 270 seconds
  no anomalies in 180 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 64 gigabytes (2^36 bytes), time= 545 seconds
  Test Name                         Raw       Processed     Evaluation
  [Low8/32]FPF-14+6/16:all          R=  +5.2  p =  2.2e-4   unusual          
  ...and 188 test result(s) without anomalies

rng=RNG_stdin32, seed=0x191622aa
length= 128 gigabytes (2^37 bytes), time= 1103 seconds
  Test Name                         Raw       Processed     Evaluation
  BCFN(2+1,13-0,T)                  R=  +9.7  p =  9.9e-5   mildly suspicious
  ...and 195 test result(s) without anomalies

rng=RNG_stdin32, seed=0x191622aa
length= 256 gigabytes (2^38 bytes), time= 2171 seconds
  no anomalies in 204 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 512 gigabytes (2^39 bytes), time= 4363 seconds
  no anomalies in 213 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 1 terabyte (2^40 bytes), time= 8698 seconds
  no anomalies in 220 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 2 terabytes (2^41 bytes), time= 17208 seconds
  no anomalies in 228 test result(s)

rng=RNG_stdin32, seed=0x191622aa
length= 4 terabytes (2^42 bytes), time= 34682 seconds
  no anomalies in 237 test result(s)
```

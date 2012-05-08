(* Files that we simply cannot parse *)
let preprocessor_fail = Token.make_table [|
  (* Unsupported local K&R function definition *)
  "testsuite/gcc/gcc.c-torture/execute/920501-7.c", ();
  "testsuite/gcc/gcc.c-torture/execute/921017-1.c", ();
  "testsuite/gcc/gcc.c-torture/execute/comp-goto-2.c", ();

  (* __m256i is redefined in an unsupported way *)
  "testsuite/gcc/gcc.target/i386/pr46253.c", ();

  (* missing headers "nonexist0.h" *)
  "testsuite/gcc/gcc.misc-tests/mg-2.c", ();
  "testsuite/gcc/gcc.misc-tests/mg.c", ();

  (* misc. errors from preprocessor *)
  "testsuite/gcc/gcc.target/i386/cleanup-2.c", ();
  "testsuite/gcc/gcc.target/i386/fma3-builtin.c", ();
  "testsuite/gcc/gcc.target/i386/fma4-builtin.c", ();
  "testsuite/gcc/gcc.target/i386/gcc-have-sync-compare-and-swap-4.c", ();
  "testsuite/gcc/gcc.target/i386/large-size-array-3.c", ();
  "testsuite/gcc/gcc.target/i386/pr25254.c", ();
  "testsuite/gcc/gcc.target/i386/pr38988.c", ();
  "testsuite/gcc/gcc.target/i386/pr45852.c", ();
|]


(* Files that we can parse, but our generated code is not accepted by GCC *)
let compiler_fail = Token.make_table [|
  "testsuite/gcc/gcc.c-torture/compile/20010701-1.c", ();
  "testsuite/gcc/gcc.c-torture/compile/20060109-1.c", ();
  "testsuite/gcc/gcc.c-torture/compile/20111209-1.c", ();
  "testsuite/gcc/gcc.c-torture/compile/920625-2.c", ();
  "testsuite/gcc/gcc.c-torture/compile/920902-1.c", ();
  "testsuite/gcc/gcc.c-torture/compile/mipscop-1.c", ();
  "testsuite/gcc/gcc.c-torture/compile/mipscop-2.c", ();
  "testsuite/gcc/gcc.c-torture/compile/mipscop-3.c", ();
  "testsuite/gcc/gcc.c-torture/compile/mipscop-4.c", ();
  "testsuite/gcc/gcc.c-torture/compile/pr43679.c", ();
  "testsuite/gcc/gcc.c-torture/compile/pr46866.c", ();
  "testsuite/gcc/gcc.c-torture/compile/pr51694.c", ();
  "testsuite/gcc/gcc.c-torture/compile/vector-1.c", ();
  "testsuite/gcc/gcc.c-torture/compile/vector-2.c", ();
  "testsuite/gcc/gcc.c-torture/compile/vector-3.c", ();
  "testsuite/gcc/gcc.c-torture/execute/930603-1.c", ();
  "testsuite/gcc/gcc.c-torture/execute/vector-compare-1.c", ();
  "testsuite/gcc/gcc.c-torture/execute/vector-compare-2.c", ();
  "testsuite/gcc/gcc.c-torture/execute/vector-subscript-1.c", ();
  "testsuite/gcc/gcc.c-torture/execute/vector-subscript-2.c", ();
  "testsuite/gcc/gcc.c-torture/execute/vector-subscript-3.c", ();
  "testsuite/gcc/gcc.c-torture/unsorted/dump-noaddr.c", ();
  "testsuite/gcc/gcc.c-torture/unsorted/ex.c", ();
  "testsuite/gcc/gcc.c-torture/unsorted/pass.c", ();
  "testsuite/gcc/gcc.c-torture/unsorted/uuarg.c", ();
  "testsuite/gcc/gcc.misc-tests/dhry.c", ();
  "testsuite/gcc/gcc.target/i386/asm-1.c", ();
  "testsuite/gcc/gcc.target/i386/attributes-error.c", ();
  "testsuite/gcc/gcc.target/i386/avx-extract-1.c", ();
  "testsuite/gcc/gcc.target/i386/avx-vcvtsi2sd-2.c", ();
  "testsuite/gcc/gcc.target/i386/avx-vcvtsi2ss-2.c", ();
  "testsuite/gcc/gcc.target/i386/avx-vmovq-2.c", ();
  "testsuite/gcc/gcc.target/i386/avx-vpinsrq-1.c", ();
  "testsuite/gcc/gcc.target/i386/bmi2-mulx64-1a.c", ();
  "testsuite/gcc/gcc.target/i386/bmi2-mulx64-1.c", ();
  "testsuite/gcc/gcc.target/i386/bmi2-mulx64-2a.c", ();
  "testsuite/gcc/gcc.target/i386/bmi2-mulx64-2.c", ();
  "testsuite/gcc/gcc.target/i386/cmpxchg16b-1.c", ();
  "testsuite/gcc/gcc.target/i386/funcspec-4.c", ();
  "testsuite/gcc/gcc.target/i386/funcspec-5.c", ();
  "testsuite/gcc/gcc.target/i386/funcspec-6.c", ();
  "testsuite/gcc/gcc.target/i386/movdi-rex64.c", ();
  "testsuite/gcc/gcc.target/i386/pr14289-1.c", ();
  "testsuite/gcc/gcc.target/i386/pr31167.c", ();
  "testsuite/gcc/gcc.target/i386/pr32280-1.c", ();
  "testsuite/gcc/gcc.target/i386/pr36786.c", ();
  "testsuite/gcc/gcc.target/i386/pr37275.c", ();
  "testsuite/gcc/gcc.target/i386/pr43528.c", ();
  "testsuite/gcc/gcc.target/i386/pr44942.c", ();
  "testsuite/gcc/gcc.target/i386/pr45296.c", ();
  "testsuite/gcc/gcc.target/i386/pr46095.c", ();
  "testsuite/gcc/gcc.target/i386/pr51393.c", ();
  "testsuite/gcc/gcc.target/i386/regparm.c", ();
  "testsuite/gcc/gcc.target/i386/rotate-2.c", ();
  "testsuite/gcc/gcc.target/i386/sse-22a.c", ();
  "testsuite/gcc/gcc.target/i386/sse-22.c", ();
  "testsuite/gcc/gcc.target/i386/sse-23.c", ();
  "testsuite/gcc/gcc.target/i386/sse-24.c", ();
  "testsuite/gcc/gcc.target/i386/sse2-cvtsi2sd-2.c", ();
  "testsuite/gcc/gcc.target/i386/sse2-extract-1.c", ();
  "testsuite/gcc/gcc.target/i386/sse2-init-v2di-2.c", ();
  "testsuite/gcc/gcc.target/i386/sse2-movq-2.c", ();
  "testsuite/gcc/gcc.target/i386/sse4_1-pinsrq.c", ();
  "testsuite/gcc/gcc.target/i386/sse-cvtsi2ss-2.c", ();
  "testsuite/gcc/gcc.target/i386/sse-vect-types.c", ();
  "testsuite/gcc/gcc.target/i386/stack-prot-kernel.c", ();
  "testsuite/gcc/gcc.target/i386/stack-prot-kernel.c", ();
  "testsuite/gcc/gcc.target/i386/vararg-1.c", ();
  "testsuite/gcc/gcc.target/i386/vararg-2.c", ();
  "testsuite/gcc/gcc.target/i386/vecinit-1.c", ();
  "testsuite/gcc/gcc.target/i386/vecinit-2.c", ();
  "testsuite/gcc/gcc.target/i386/vecinit-3.c", ();
  "testsuite/gcc/gcc.target/i386/vecinit-4.c", ();
  "testsuite/gcc/gcc.target/i386/vecinit-5.c", ();
  "testsuite/gcc/gcc.target/i386/vecinit-6.c", ();
  "testsuite/gcc/gcc.target/i386/xorps-sse2.c", ();
  "testsuite/gcc/gcc.target/i386/xorps-sse.c", ();
|]


let preprocessor_fail file =
  try
    Hashtbl.find preprocessor_fail file; true
  with Not_found ->
    false


let compiler_fail file =
  try
    Hashtbl.find compiler_fail file; true
  with Not_found ->
    false

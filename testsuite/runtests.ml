(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  { empty with
    tool = "fcc1.native";
    suffixes = [".c"];
    options = Some ("-w -cflags '-I" ^ Sys.getenv "includedir" ^ "'");
    dirs = [
      "cparser";
    ];
  };
])

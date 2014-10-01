(** Register some compilers that have Stacki as a target or source language. *)
let initialize () =
  Compilers.register "stacki" "stacki" (module Compilers.Identity (Stacki));
  Compilers.register "fopi"   "stacki" (module FopiToStacki)

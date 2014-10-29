(** Register some compilers that have Stackix as a target or source language. *)
let initialize () =
  Compilers.register "stackix" "stackix" (module Compilers.Identity (Stackix));
  Compilers.register "fopix"   "stackix" (module FopixToStackix)

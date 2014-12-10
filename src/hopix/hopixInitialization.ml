let initialize () =
  Compilers.register "hopix" "hopix" (module Compilers.Identity (Hopix));
  Compilers.register "hopix" "datix" (module HopixToDatix)

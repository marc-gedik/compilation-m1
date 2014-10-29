let initialize () =
  Compilers.register "fopix" "fopix" (module Compilers.Identity (Fopix));
  Compilers.register "datix" "fopix" (module DatixToFopix)

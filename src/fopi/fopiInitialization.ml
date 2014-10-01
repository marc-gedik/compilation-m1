let initialize () =
  Compilers.register "fopi" "fopi" (module Compilers.Identity (Fopi))

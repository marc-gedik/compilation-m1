(** Command line arguments analysis. *)

let options_list =
  ref []

let push local_options =
  options_list := !options_list @ local_options

let options names kind doc =
  List.map (fun n -> (n, kind, doc)) names

let show_version_and_exits () =
  Printf.printf "flap %s\n%!" Version.number;
  exit 0

let generic_options = Arg.(align (List.flatten [
  options
    ["--version"; "-v"]
    (Unit show_version_and_exits)
    " Show the version number and exits.";

  options
    ["--source"; "-s"]
    (String Options.set_source_language)
    (" Set the source programming language");

  options
    ["--target"; "-t"]
    (String Options.set_target_language)
    (" Set the target programming language");

  options
    ["--interactive"; "-i"]
    (Bool Options.set_interactive_mode)
    ("(true|false) Set the compiler mode");

  options
    ["--verbose"; "-V"]
    (Bool Options.set_verbose_mode)
    (" Ask the compiler to be verbose")

]))

let usage_msg =
  "flap [options] input_filename"

let parse () =
  Arg.parse !options_list Options.set_input_filename usage_msg

let initialize =
  push generic_options

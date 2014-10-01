let prompt = ref ""

let set_prompt = ( := ) prompt

let print_prompt () =
  output_string stdout !prompt;
  flush stdout

let input_char =
  let display_prompt  = ref true in
  let buffer = "0" in
  let ask stdin =
    if !display_prompt then begin
      display_prompt := false;
      print_prompt ()
    end;
    let c = input_char stdin in
      if c = '\n' then display_prompt := true;
      buffer.[0] <- c;
      buffer
  in
    ask

let set_ascii () = ()

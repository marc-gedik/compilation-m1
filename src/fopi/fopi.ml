(** The fopi programming language. *)

module AST = FopiAST

type ast = FopiAST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:FopiLexer.token
    ~parser_fun:FopiParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".fopi"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  FopiPrettyPrinter.(to_string program ast)

include FopiInterpreter

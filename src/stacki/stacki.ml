(** The stacki programming language. *)

module AST = StackiAST

type ast =
    StackiAST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:StackiLexer.token
    ~parser_fun:StackiParser.program
    ~input

let parse_filename filename =
  parse Lexing.from_channel (open_in filename)

let extension =
  ".stacki"

let parse_string =
  parse Lexing.from_string

let print_ast ast =
  StackiPrettyPrinter.(to_string program ast)

include StackiInterpreter

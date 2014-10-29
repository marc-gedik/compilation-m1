%{

  open DatixAST

%}

%token VAL DEF IN END IF THEN ELSE EVAL WITH CASE TYPE
%token PLUS MINUS STAR SLASH GT GTE LT LTE EQUAL
%token UNDERSCORE RIGHTARROW PIPE DOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token ASSIGNS COMMA SEMICOLON COLON EOF
%token<int> INT
%token<string> ID UID

%right SEMICOLON
%nonassoc ASSIGNS
%nonassoc GT GTE LT LTE EQUAL
%left PLUS MINUS
%left STAR SLASH
%left DOT
%nonassoc LBRACKET

%start<DatixAST.t> program

%%

program: ds=located(definition)* EOF
{
  ds
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}

definition: VAL x=located(pattern) EQUAL e=located(expression)
{
  DefineValue (x, e)
}
| DEF f=located(function_identifier)
  xs=formals
  out_ty=type_annotation
  EQUAL e=located(expression)
{
  DefineFunction (f, xs, out_ty, e)
}
| EVAL e=located(expression)
{
  DefineValue (Position.map (fun _ -> PVariable (Id "res")) e, e)
}
| TYPE t=type_identifier EQUAL td=type_definition
{
  DefineType (t, td)
}

type_definition:
  LBRACE fs=separated_nonempty_list(SEMICOLON, fielddef) RBRACE
{
  RecordTy fs
}
| PIPE? ts=separated_nonempty_list(PIPE, tagdef)
{
  TaggedUnionTy ts
}

tagdef:
  k=tag LPAREN ts=separated_list(COMMA, typ) RPAREN
{
  (k, ts)
}

fielddef:
  l=label COLON t=typ
{
  (l, t)
}

%inline formals: LPAREN xs=separated_list(COMMA, binding) RPAREN {
  xs
}

binding: x=identifier COLON ty=typ
{
  (x, ty)
}

expression:
  l=literal
{
  Literal l
}
| x=identifier
{
  Variable x
}
| VAL x=located(pattern)
  EQUAL
    e1=located(expression)
  IN
    e2=located(expression)
  END
{
  Define (x, e1, e2)
}
| IF
  c=located(expression)
  THEN t=located(expression)
  ELSE f=located(expression)
  END
{
  IfThenElse (c, t, f)
}
| f=function_identifier
  LPAREN es=separated_list(COMMA, located(expression)) RPAREN
{
  FunCall (f, es)
}
| l=located(expression) b=binop r=located(expression) {
  FunCall (FunId b, [l; r])
}
| e=located(expression) LBRACKET i=located(expression) RBRACKET {
  FunCall (FunId "block_get", [e; i])
}
| e=located(expression)
  LBRACKET i=located(expression) RBRACKET
  ASSIGNS v=located(expression) {
  FunCall (FunId "block_set", [e; i; v])
}
| e1=located(expression) SEMICOLON e2=located(expression) {
  Define (Position.map (fun _ -> PWildcard) e1, e1, e2)
}
| LPAREN es=separated_nonempty_list(COMMA, located(expression)) RPAREN {
  match es with
    | [] -> assert false
    | [e] -> Position.value e
    | es -> Tuple es
}
| CASE e=located(expression)
  WITH
  PIPE? bs=separated_nonempty_list(PIPE, branch)
  END
{
  Case (e, bs)
}
| LBRACE fs=separated_nonempty_list(SEMICOLON, field) RBRACE
{
  Record fs
}
| e=located(expression) DOT l=label
{
  RecordField (e, l)
}
| k=tag LPAREN es=separated_list(COMMA, located(expression)) RPAREN
{
  TaggedValues (k, es)
}

field: l=label EQUAL e=located(expression)
{
  (l, e)
}

label: x=ID
{
  Label x
}

branch: p=located(pattern) RIGHTARROW e=located(expression)
{
  Branch (p, e)
}

pattern: x=identifier
{
  PVariable x
}
| UNDERSCORE
{
  PWildcard
}
| LPAREN ps=separated_nonempty_list(COMMA, identifier) RPAREN
{
  PTuple ps
}
| k=tag LPAREN ps=separated_list(COMMA, identifier) RPAREN
{
  PTaggedValues (k, ps)
}

tag: k=UID
{
  Constructor k
}

%inline type_annotation: /* empty */
{
  None
}
| COLON ty=typ
{
  Some ty
}

typ:
  x=type_identifier
{
  TyIdentifier x
}
| lhs=typ STAR rhs=typ
{
  match rhs with
    | TyTuple ts -> TyTuple (lhs :: ts)
    | _ -> TyTuple [lhs; rhs]
}
| LPAREN t=typ RPAREN
{
  t
}

type_identifier: x=ID
{
  TId x
}

%inline binop:
  PLUS  { "+"  }
| MINUS { "-"  }
| STAR  { "*"  }
| SLASH { "/"  }
| GT    { ">"  }
| GTE   { ">=" }
| LT    { "<"  }
| LTE   { "<=" }
| EQUAL { "="  }

%inline literal:
  x=INT
{
  LInt x
}

%inline identifier: x=ID {
  Id x
}

%inline function_identifier: x=ID {
  FunId x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

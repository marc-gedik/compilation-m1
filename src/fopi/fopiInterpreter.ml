open Position
open Error
open FopiAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of fopi evaluates into a [value]. *)
type value =
  | VUnit
  | VInt      of int
  | VBool     of bool
  | VLocation of Memory.location

type 'a coercion = value -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_location = function VLocation x -> Some x | _ -> None
let value_as_unit     = function VUnit -> Some () | _ -> None

type 'a wrapper = 'a -> value
let int_as_value x  = VInt x
let bool_as_value x = VBool x
let location_as_value x = VLocation x
let unit_as_value () = VUnit

let print_value = function
  | VInt x      -> string_of_int x
  | VBool true  -> "true"
  | VBool false -> "false"
  | VUnit       -> "()"
  | VLocation l -> Memory.print_location l

module Environment : sig
  type t
  val initial : t
  val bind    : t -> identifier -> value -> t
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> value
  val last    : t -> (identifier * value * t) option
  val print   : t -> string
end = struct
  type t = (identifier * value) list

  let initial = []

  let bind e x v = (x, v) :: e

  exception UnboundIdentifier of identifier

  let lookup x e =
    try
      List.assoc x e
    with Not_found ->
      raise (UnboundIdentifier x)

  let last = function
    | [] -> None
    | (x, v) :: e -> Some (x, v, e)

  let print_binding (Id x, v) =
    x ^ " = " ^ print_value v

  let print env =
    String.concat "\n" (List.map print_binding env)

end

type runtime = {
  environment : Environment.t;
}

type observable = {
  new_environment : Environment.t;
}

let initial_runtime () = {
  environment = Environment.initial;
}

(** 640k ought to be enough for anybody -- B.G. *)
let memory : value Memory.t = Memory.create (640 * 1024)

let rec evaluate runtime ast =
  let runtime' = List.fold_left declaration runtime ast in
  (runtime', extract_observable runtime runtime')


and declaration runtime = function
  | DefineValue (i, e) ->
    let v = expression' runtime e in
    let i = Position.value i in
    { runtime with environment = Environment.bind runtime.environment i v }
  | DefineFunction _ ->
    runtime

and expression' runtime e =
  expression (position e) runtime (value e)

and arith_operator_of_symbol = function
  | "+" -> ( + )
  | "-" -> ( - )
  | "/" -> ( / )
  | "*" -> ( * )
  | _ -> assert false

and cmp_operator_of_symbol = function
  | "<" -> ( < )
  | ">" -> ( > )
  | "<=" -> ( <= )
  | ">=" -> ( >= )
  | "=" -> ( = )
  | _ -> assert false

and evaluation_of_binary_symbol environment = function
  | ("+" | "-" | "*" | "/") as s ->
    arith_binop environment (arith_operator_of_symbol s)
  | ("<" | ">" | "<=" | ">=" | "=") as s ->
    arith_cmpop environment (cmp_operator_of_symbol s)
  | _ -> assert false

and is_binary_primitive = function
  | "+" | "-" | "*" | "/" | "<" | ">" | "<=" | ">=" | "=" -> true
  | _ -> false

and expression position runtime = function
  | Literal l ->
    literal l

  | Variable x ->
    Environment.lookup x runtime.environment

  | IfThenElse (c, t, f) ->
    failwith "Student! This is your job!"

  | Define (x, ex, e) ->
    let v = expression' runtime ex in
    let runtime = { runtime with
      environment = Environment.bind runtime.environment (Position.value x) v
    }
    in
    expression' runtime e

  | FunCall (FunId "block_create", [size; init]) ->
    failwith "Student! This is your job!"

  | FunCall (FunId "block_get", [location; index]) ->
    failwith "Student! This is your job!"

  | FunCall (FunId "block_set", [location; index; e]) ->
    failwith "Student! This is your job!"

  | FunCall (FunId s, [e1; e2]) when is_binary_primitive s ->
    evaluation_of_binary_symbol runtime s e1 e2


and binop
: type a b. a coercion -> b wrapper -> _ -> (a -> a -> b) -> _ -> _ -> value
= fun coerce wrap runtime op l r ->
  let lv = expression' runtime l
  and rv = expression' runtime r in
  match coerce lv, coerce rv with
    | Some li, Some ri ->
      wrap (op li ri)
    | _, _ ->
      error
        [position l; position r]
        "Invalid binary operation."

and arith_binop env = binop value_as_int int_as_value env
and arith_cmpop env = binop value_as_int bool_as_value env

and literal = function
  | LInt x -> VInt x

and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.initial runtime.environment runtime'.environment
  }

let print_observable runtime observation =
  Environment.print observation.new_environment

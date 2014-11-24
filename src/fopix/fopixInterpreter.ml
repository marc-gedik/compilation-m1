open Position
open Error
open FopixAST

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
    (* Identifiers starting with '_' are reserved for the compiler.
       Their values must not be observable by users. *)
    if x.[0] = '_' then
      ""
    else
      x ^ " = " ^ print_value v

  let print env =
    String.concat "\n" (
      List.(filter (fun s -> s <> "") (map print_binding env))
    )

end

module FunEnv =
  Map.Make(
      struct
	type t = function_identifier
	let compare = Pervasives.compare
      end
    )

type runtime = {
  environment : Environment.t;
  funEnvironment : (formals*expression located) FunEnv.t;
}

type observable = {
  new_environment : Environment.t;
}

let initial_runtime () = {
  environment = Environment.initial;
  funEnvironment = FunEnv.empty;
}

(** 640k ought to be enough for anybody -- B.G. *)
let memory : value Memory.t = Memory.create (640 * 1024)

let bind_functions runtime = function
  | DefineFunction (f, formals, e) ->
     let f = Position.value f in
     { runtime with funEnvironment = FunEnv.add f (formals, e) runtime.funEnvironment}
  | _ -> runtime

let rec evaluate runtime ast =
  let runtime = List.fold_left bind_functions runtime ast in
  let runtime' = List.fold_left declaration runtime ast in
  (runtime', extract_observable runtime runtime')


and declaration runtime = function
  | DefineValue (i, e) ->
    let v = expression' runtime e in
    let i = Position.value i in
    { runtime with environment = Environment.bind runtime.environment i v }
  | DefineFunction (f, formals, e) ->
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
    ifThenElse runtime c t f

  | Define (x, ex, e) ->
    let v = expression' runtime ex in
    let runtime' = { runtime with
      environment = Environment.bind runtime.environment (Position.value x) v
    }
    in
    expression' runtime' e

  (** block_create (size, init) (init -> valeur dans chaque case) *)
  | FunCall (FunId "block_create", [size; init]) ->
    let size = expression' runtime size in
    let init = expression' runtime init in
    VLocation (block_create size init)

  (** block_get (block, index) *)
  | FunCall (FunId "block_get", [location; index]) ->
    let location = expression' runtime location in
    let index = expression' runtime index in
    block_get location index

  (** block_set block index valeur *)
  | FunCall (FunId "block_set", [location; index; e]) ->
    let location = expression' runtime location in
    let index = expression' runtime index in
    let e = expression' runtime e in
    block_set location index e;
    VUnit

  | FunCall (FunId s, [e1; e2]) when is_binary_primitive s ->
    evaluation_of_binary_symbol runtime s e1 e2

  | FunCall (FunId id as f , args) ->
     (try
	 let formals, expr = FunEnv.find f runtime.funEnvironment in
	 let runtime = bind_args formals args runtime in
	 expression' runtime expr
       with Not_found ->
	 raise (error [position] (Printf.sprintf "Unbound Function Identifier %s" id))
     )
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

       and ifThenElse runtime c t f =
	 let expr =
	   match expression' runtime c with
	   | VBool b when b = true -> t
	   | VBool _ -> f
	   | _ as value -> failwith (print_value value ^ " is not a bool")
	 in
	 expression' runtime expr

       and block_create (VInt size) init =
	 Memory.allocate memory size init

       and block_get (VLocation location) (VInt index) =
	 Memory.read (Memory.dereference memory location) index

       and block_set (VLocation location) (VInt index) e =
	 Memory.write (Memory.dereference memory location) index e

       and bind_args formals args runtime =
	 match formals, args with
	 | [], [] -> runtime
	 | _::_, [] | [], _::_ -> failwith "TODO raise mauvais arguments"
	 | var::formals, value::args ->
	    let value = expression' runtime value in
	    let runtime = { runtime with
			    environment = Environment.bind runtime.environment var value
			  }
       in bind_args formals args runtime

let print_observable runtime observation =
  Environment.print observation.new_environment

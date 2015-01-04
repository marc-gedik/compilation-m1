open Position
open Error
open HopixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of datix evaluates into a [value]. *)
type 'e gvalue =
  | VInt       of int
  | VBool      of bool
  | VTuple     of 'e gvalue list
  | VRecord    of (label * 'e gvalue) list
  | VTagged    of tag * 'e gvalue list
  | VClosure   of 'e * lambda
  | VPrimitive of ('e gvalue -> 'e gvalue)

type ('a, 'e) coercion = 'e gvalue -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_record   = function VRecord x -> Some x | _ -> None
let value_as_tagged   = function VTagged (t, x) -> Some (t, x) | _ -> None
let value_as_tuple    = function VTuple vs -> Some vs | _ -> None

type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x
let bool_as_value x = VBool x
let record_as_value x = VRecord x
let tagged_as_value t x = VTagged (t, x)
let tuple_as_value ts = VTuple ts

let primitive ?(error = fun () -> assert false) coercion wrapper f =
  VPrimitive (fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

let rec print_value = function
  | VInt x          -> string_of_int x
  | VBool true      -> "true"
  | VBool false     -> "false"
  | VTuple vs       -> "(" ^ String.concat ", " (List.map print_value vs) ^ ")"
  | VRecord r       -> "{" ^ String.concat "; " (List.map print_field r) ^ "}"
  | VTagged (t, vs) -> tag t ^ "(" ^ String.concat ", " (List.map print_value vs) ^ ")"
  | VClosure (_, _)
  | VPrimitive _    -> "<fun>"

and print_field (Label l, v) =
  l ^ " = " ^ print_value v

and tag (Constructor id) =
  id

module Environment : sig
  type t
  val empty : t
  val bind    : t -> identifier -> t gvalue -> t
  val update  : identifier -> t -> t gvalue -> unit
  exception UnboundIdentifier of identifier
  val lookup  : identifier -> t -> t gvalue
  val last    : t -> (identifier * t gvalue * t) option
  val print   : t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier

  let lookup' x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier x)
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup x e = !(lookup' x e)

  let update x e v =
    lookup' x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding (Id x, v) =
    x ^ " = " ^ print_value !v

  let print e =
    let b = Buffer.create 13 in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, e) -> Buffer.add_string b (print_binding (x, v) ^ "\n"); aux e
    in
    aux e

end

type value = Environment.t gvalue

type formals = identifier list

type runtime = {
  environment : Environment.t;
}

type observable = {
  new_environment : Environment.t;
}

let arith_operator_of_symbol = function
  | "+" -> ( + )
  | "-" -> ( - )
  | "/" -> ( / )
  | "*" -> ( * )
  | _ -> assert false
let cmp_operator_of_symbol = function
  | "<"  -> ( < )
  | ">"  -> ( > )
  | "<=" -> ( <= )
  | ">=" -> ( >= )
  | "="  -> ( = )
  | _ -> assert false


(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let arith_operators = ["+"; "-"; "/"; "*"] in
  let cmp_operators   = ["<"; ">"; "<="; ">="; "="] in


  let int_int_int f = primitive value_as_int
			    (primitive value_as_int int_as_value)
			    f
  in
  let int_int_bool f = primitive value_as_int
			    (primitive value_as_int bool_as_value)
			    f
  in
  let env = Environment.empty in
  let env = List.fold_left
	      (fun env x -> Environment.bind env (Id x) (int_int_int (arith_operator_of_symbol x)))
	      env arith_operators in
  let env = List.fold_left
	      (fun env x -> Environment.bind env (Id x) (int_int_bool (cmp_operator_of_symbol x)))
	      env cmp_operators in
  env

let initial_runtime () = {
  environment = primitives;
}

(** 640k ought to be enough for anybody -- B.G. *)
let memory : value Memory.t = Memory.create (640 * 1024)

exception MatchFailure

let rec evaluate runtime ast =
  let runtime' = List.fold_left declaration runtime ast in
  (runtime', extract_observable runtime runtime')

and declaration runtime d =
  match Position.value d with
  | DefineValue (pat, e) ->
     bind_pattern runtime pat (expression' runtime e)

  | DefineType _ ->
     runtime

and expression' runtime e =
  expression (position e) runtime (value e)

and expression position runtime = function
  | Fun (x, e) ->
       failwith "Student! This is your job!26"

  | Apply (a, b) ->
       failwith "Student! This is your job!27"

  | RecFuns fs ->
       failwith "Student! This is your job!28"

  | RecordField (e, l) ->
     let VRecord recs = expression' runtime e in
     let value = List.assoc l recs in
     value

  | Tuple es ->
     let values = List.map (expression' runtime) es in
     VTuple values

  | Record rs ->
     let recs = List.map (fun (x,y) -> (x,(expression' runtime y))) rs in
     VRecord recs

  | TaggedValues (k, es) ->
     let values = List.map (expression' runtime) es in
     VTagged (k,values)

  | Case (e, bs) ->
     branches runtime (expression' runtime e) bs

  | Literal l ->
     literal l

  | Variable x ->
     Environment.lookup x runtime.environment

  | Define (pat, ex, e) ->
     let v = expression' runtime ex in
     expression' (bind_pattern runtime pat v) e

  | IfThenElse (c, t, f) ->
     ifThenElse runtime c t f

and branches runtime v = function
  | [] ->
       assert false (* by typing *)

  | Branch (pat, e) :: bs ->
         (try
	 let runtime = bind_pattern runtime pat v in
	 expression' runtime e
       with _ -> branches runtime v bs
     )

and bind_variable runtime x v =
  { environment = Environment.bind runtime.environment x v }

and bind_pattern runtime pat v : runtime =
  match Position.value pat, v with
    | PWildcard, _ ->
      runtime

    | PVariable x, _ ->
       bind_variable runtime x v

    | PTuple xs, VTuple vs ->
       List.fold_left2 bind_variable runtime xs vs

    | PTaggedValues (k, xs), VTagged (k', vs) ->
       if k = k'
       then List.fold_left2 bind_variable runtime xs vs
       else failwith ("Tag are not the same" ^ (tag k) ^ " vs " ^ (tag k'))

    | _, _ ->
      assert false (* By typing. *)

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
      substract Environment.empty runtime.environment runtime'.environment
  }

and ifThenElse runtime c t f =
  let expr =
    match expression' runtime c with
    | VBool b when b = true -> t
    | VBool _ -> f
    | _ -> assert false
  in
  expression' runtime expr

let print_observable runtime observation =
  Environment.print observation.new_environment

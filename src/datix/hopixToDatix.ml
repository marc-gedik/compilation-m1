module Source = Hopix
module S = Source.AST
module Target = Datix
module T = Target.AST

type closure_environment = {
  variables  : (S.identifier * S.expression) list;
}

let empty_environment = {
  variables = [];
}

let lookup_environment env x =
  List.assoc x env.variables

let bind env x e = {
  variables = (x, e) :: env.variables
}

let bind_local env x =
  bind env x (S.Variable x)

type environment = ()

let initial_environment _ = ()

let fresh_var =
  let c = ref 0 in
  fun ?(prefix="x") () -> incr c; S.Id (prefix ^ string_of_int !c)

let fresh_env_var =
  fresh_var ~prefix:"_env"

let fresh_vars =
  let rec aux accu i =
    if i = 0 then accu else aux (fresh_var () :: accu) (pred i)
  in
  aux []

let closure_conversion : HopixAST.t -> HopixAST.t =

  let open HopixAST in
  let located f x = Position.(map (f (position x)) x) in
  let locate = Position.with_pos in

  let proj pos what idx over =
       failwith "Student! This is your job!"
  in


  let rec program p =
    List.map (located definition) p

  and definition pos = function
  | DefineValue (p, e) ->
       failwith "Student! This is your job!"

  | DefineType (tid, tdef) ->
       failwith "Student! This is your job!"

  and expression e =
    expression_aux empty_environment e

  and expression_aux env pos = function
  | Literal l ->
       failwith "Student! This is your job!"

  | Variable x ->
       failwith "Student! This is your job!"

  | Define (p, e1, e2) ->
       failwith "Student! This is your job!"

  | Tuple es ->
       failwith "Student! This is your job!"

  | Record rs ->
       failwith "Student! This is your job!"

  | RecordField (e, f) ->
       failwith "Student! This is your job!"

  | TaggedValues (k, es) ->
       failwith "Student! This is your job!"

  | IfThenElse (a, b, c) ->
       failwith "Student! This is your job!"

  | Case (e, bs) ->
       failwith "Student! This is your job!"

  | Apply (e1, e2) ->
       failwith "Student! This is your job!"

  | Fun ((x, ty), e) as l ->
       failwith "Student! This is your job!"

  | RecFuns rfs ->
       failwith "Student! This is your job!"

  and branch env (Branch (p, e)) =
       failwith "Student! This is your job!"

  and bind_pattern env p =
       failwith "Student! This is your job!"


  in
  program

let hoist : HopixAST.t -> DatixAST.t * environment =
  fun p ->
       failwith "Student! This is your job!"

let translate p env =
  hoist (closure_conversion p)

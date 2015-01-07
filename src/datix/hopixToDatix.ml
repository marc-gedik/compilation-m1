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
       failwith "Student! This is your job!45"
  in


  let rec program p =
    List.map (located definition) p

  and pattern p =
    failwith  "pattern"

  and definition pos = function
  | DefineValue (p, e) ->
     T.DefineValue (pattern p, expression pos (Position.value e))

  | DefineType (tid, tdef) ->
       failwith "Student! This is your job!47"

  and expression' env e =
    expression_aux env (Position.position e) (Position.value e)

  and expression e =
    expression_aux empty_environment e

  and expression_aux env pos = function
  | Literal (LInt l) ->
     locate pos (T.Literal (T.LInt l))

  | Variable (Id x) ->
     locate pos (T.Variable (T.Id x))

  | Define (p, e1, e2) ->
     let p = pattern p in
     let e1 = expression' env e1 in
     let e2 = expression' env e2 in
     locate pos (T.Define (p, e1, e2))

  | Tuple es ->
     locate pos (T.Tuple (List.map (expression' env) es))

  | Record rs ->
       failwith "Student! This is your job!52"

  | RecordField (e, f) ->
       failwith "Student! This is your job!53"

  | TaggedValues (k, es) ->
       failwith "Student! This is your job!54"

  | IfThenElse (a, b, c) ->
       failwith "Student! This is your job!55"

  | Case (e, bs) ->
       failwith "Student! This is your job!56"

  | Apply (e1, e2) ->
       failwith "Student! This is your job!57"

  | Fun ((x, ty), e) as l ->
       failwith "Student! This is your job!58"

  | RecFuns rfs ->
       failwith "Student! This is your job!59"

  and branch env (Branch (p, e)) =
       failwith "Student! This is your job!60"

  and bind_pattern env p =
       failwith "Student! This is your job!61"


  in
  program

let hoist : HopixAST.t -> DatixAST.t * environment =
  fun p ->
       failwith "Student! This is your job!62"

let translate p env =
  hoist (closure_conversion p)

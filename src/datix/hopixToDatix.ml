module Source = Hopix
module S = Source.AST
module Target = Datix
module T = Target.AST
open Position

(** A closure compilation environment relates an identifier to the way
    it is accessed in the compiled version of the function's
    body.

    Indeed, consider the following example. Imagine that the following
    function is to be compiled:

    fun x -> x + y

    In that case, the closure compilation environment will
    contain:

    x -> x
    y -> "the code that extract the value of y from the closure environment"

    Indeed, "x" is a local variable that can be accessed directly in
    the compiled version of this function's body whereas "y" is a free
    variable whose value must be retrieved from the closure's
    environment.

 *)
type closure_environment = {
  variables  : (S.identifier * S.expression) list;
}

let empty_environment = {
  variables = [];
}

let lookup_environment env x =
  try
    List.assoc x env.variables
  with Not_found ->
    let S.Id s = x in
    let _ =  Printf.printf "%s : " s in
    raise Not_found

let bind env x e = {
  variables = (x, e) :: env.variables
}

let bind_local env x =
  bind env x (S.Variable x)

(** We do not try to produce a well-typed compiled program.
    Therefore, we put a dummy type at every place where a type
    is required.
 *)
let tydummy = T.(TyIdentifier (TId "d"))

(** There is no need for a global environment for this
    compilation pass. *)
type environment = ()

let initial_environment _ = ()

(** [fresh_var ?prefix ()] produces a fresh identifier starting
    with prefix. (prefix = "x" is the default value of prefix if
    unspecified.) *)
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

type application_kind =
  | PrimitiveApplication of HopixAST.identifier * HopixAST.expression located list
  | GeneralApplication of HopixAST.expression located * HopixAST.expression located

(** [classify_application e] determines if the application [e] is
    a primitive application or a general application. *)
let classify_application e =
  match e with
  | S.Apply (e1, e2) ->
     begin  match Position.value e1 with
            | S.Apply (e0, e1') ->
               begin match Position.value e0 with
		     | S.Variable (S.Id x) when S.is_binary_primitive x ->
			PrimitiveApplication (S.Id x, [e1'; e2])
		     | _ ->
			GeneralApplication (e1, e2)
               end
            | _ ->
               GeneralApplication (e1, e2)
     end
  | _ ->
     assert false (* By precondition. *)

let locate = Position.with_pos

(** [applys pos f [e1; ...; eN]] builds a chain of applications corresponding
    to [f e1 ... eN]. *)
let applys pos f es =
  let rec aux = S.(function
		    | [] ->
		       locate pos (Variable f)
		    | e :: es ->
		       locate pos (Apply (aux es, e)))
  in
  Position.value (aux (List.rev es))


(** [closure_conversion p] turns a Hopix program [p] that contains
    anonymous functions into a Hopix program [p'] where each anonymous
    function has been changed into a closure, that is a pair
    consisting in an environment and a closed anonymous function. (A
    function is closed if it does not contain any free variables). *)
let closure_conversion : HopixAST.t -> HopixAST.t =
  HopixAST.(

    let located f x = Position.(map (f (position x)) x) in

    (** [proj pos what idx n] is the code that extracts from [what] the
      component [idx] of a tuple of size [n]. *)
    let proj pos what idx over =
      let id = fresh_var () in
      let rec aux i =
	let x = if i = idx
		then id
		else Id "_"
	in
	if i = 0
	then [x]
	else x::(aux (i-1))
      in
      let ps = locate pos (PTuple (aux (over-1))) in
      let es = locate pos (Variable id) in
      locate pos (Case(what, [Branch(ps, es)]))
    in


    let rec program p =
      List.map (located definition) p

    and definition pos = function
      | DefineValue (p, e) ->

	 let env = bind_pattern empty_environment p in
	 let e = expression' env e in
	 DefineValue (p, e)

      | DefineType (tid, tdef) ->
	 DefineType (tid, tdef)

    and expression' env e =
      expression_aux env (Position.position e) (Position.value e)

    and expression e =
      expression_aux empty_environment (Position.position e) (Position.value e)

    (** [expression_aux env pos e] translates an expression [e]
      into a compiled expression [e]. If [e] is inside the
      body of a function then [env] is not empty and contains
      the compiled code for the identifier occurring in [e]. *)
    and expression_aux env pos e =
      let locate = Position.with_pos pos in
      let aux = function
	| MutateTuple _ ->
	   failwith "Student! This is your job!52"

	| Literal l ->
	   Literal l

	| Variable x ->
	   (try lookup_environment env x
	   with Not_found -> Variable x)

	| Define (p, e1, e2) ->
	   let e1 = expression' env e1 in
	   let env = bind_pattern env p in
	   let e2 = expression' env e2 in
	   Define (p, e1, e2)

	| Tuple es ->
	   Tuple (List.map (expression' env) es)

	| Record rs ->
	   Record (List.map (fun (lbl, e) -> (lbl, expression' env e)) rs)

	| RecordField (e, f) ->
	   RecordField (expression' env e, f)

	| TaggedValues (k, es) ->
	   TaggedValues (k, List.map (expression' env) es)

	| IfThenElse (a, b, c) ->
	   let a = expression' env a in
	   let b = expression' env b in
	   let b = expression' env b in
	   IfThenElse (a, b, c)

	| Case (e, bs) ->
	   Case (expression' env e, List.map (branch env) bs)

	| Apply (e1, e2) as e ->
	   (match classify_application e with
	    | PrimitiveApplication (id, l) ->
	       applys pos id (List.map (expression' env) l)
	    | GeneralApplication (e1, e2) ->
	       let e1 = expression' env e1 in
	       let e2 = expression' env e2 in

	       let id = fresh_var () in
	       let pId = Position.with_pos pos (PVariable id) in
	       let xId = locate (Variable id) in
               let fst = proj pos e1 0 2 in
               let snd = proj pos e1 1 2 in


	       Define (pId, locate (Apply (snd, fst)), xId)
	   )

	| Fun ((x, ty), e) as l ->
	   let freeVariables = free_variables l in
	   let closure_vars = List.map (fun x -> Variable x |> locate) freeVariables in

	   let len = List.length freeVariables in

	   let fun_env = bind_local empty_environment x in
	   let _env = fresh_env_var () in
	   let xEnv = locate (Variable _env) in

	   let variables = List.mapi (fun i x -> Position.value (proj pos xEnv i len)) freeVariables in
	   let fun_env = List.fold_left2 bind fun_env freeVariables variables in

	   let closure_vars = locate (Tuple closure_vars) in

	   let e = expression' fun_env e in

	   (* fun _env (x:y) -> e *)
	   let _fun = locate (Fun ((_env, None), locate (Fun ((x, ty), e)))) in

	   Tuple [closure_vars; _fun]

	| RecFuns rfs ->
	   failwith "Student! This is your job!59"
      in
      locate (aux e)

    (**

          rec f0 = fun y1 -> e1 and ... and fN = fun yN -> eN

       is compiled into:

          let e = (dummyf1, ..., dummyfN, x1, .., xM) in
          let f0 = (e, fun env y1 -> C[e0]) in
          ...
          let fN = (e, fun env yN -> C[eN]) in
          e[0] <- f0;
          ...
          e[N] <- fN;
          (f0, ..., fN)

       where :

       - dummyfI are dummy values put temporarily at the location where
         the final closures will be stored ;

       - x1 ... XM are all the free variables of the mutually
         recursive functions (minus the names of the currently
         defined functions).

       - C[.] is a recursive call to the closure conversion function
         (with the appropriate environment).

     *)

    and branch env (Branch (p, e)) =
      let env = bind_pattern env p in
      let e = expression' env e in
      Branch(p, e)

    and bind_pattern env p =
      match Position.value p with
      | PWildcard -> env
      | PVariable id -> bind_local env id
      | PTuple ids
      | PTaggedValues (_, ids) -> List.fold_left bind_local env ids
    in
    program
  )

(** [hoist p] returns a Datix program from a closed Hopix program [p].

    It goes through [p] to give a name to every anonymous function.
    These functions are defined at toplevel in the target Datix
    programs.
 *)
let hoist : HopixAST.t -> DatixAST.t =
  fun p ->
  let located f x = Position.(map (f (position x)) x) in
  let locate = Position.with_pos in

  (** We store the newly defined functions in a list of declarations. *)
  let function_counter = ref 0 in
  let function_definitions = ref [] in
  let push_new_function pos xs e =
    let fid = incr function_counter; T.FunId ("_f" ^ string_of_int !function_counter) in
    let fdef = T.DefineFunction (locate pos fid, xs, None, e) in
    function_definitions := locate pos fdef :: !function_definitions;
    fid
  in

  let rec program p =
    (** We first translate the definition of the input program... *)
    let pc = List.(flatten (map definition p)) in
    (** ... and we put the newly defined functions in the preamble. *)
    !function_definitions
    @ pc

  and definition e =
    let pos = Position.position e in
    match Position.value e with
    | S.DefineValue (p, e) ->
       let p = pattern p in
       let e = expression e in
       [locate pos (T.DefineValue (p, e))]

    | S.DefineType (tid, tdef) ->
       []

  and expression e =
    expression_aux (Position.position e) (Position.value e)

  (** [expression_aux pos e] translates an expression [e]
      into a compiled expression [e]. *)
  and expression_aux pos e =
    let aux = function
      | S.MutateTuple _ ->
	 failwith "Student! This is your job! : MutatTuple"

      | S.Literal l ->
	 T.Literal (literal l)

      | S.Variable (S.Id x) ->
	 T.Variable (T.Id x)

      | S.Define (p, e1, e2) ->
	 let p = pattern p in
	 let e1 = expression e1 in
	 let e2 = expression e2 in
	 T.Define (p, e1, e2)


      | S.Tuple es ->
	 T.Tuple (List.map expression es)

      | S.Record rs ->
	 let rs = List.map (fun (S.Label lbl, e) -> T.Label lbl, expression e) rs in
	 T.Record rs

      | S.RecordField (e, S.Label f) ->
	 let e = expression e in
	 T.RecordField (e, T.Label f)

      | S.TaggedValues (S.Constructor k, es) ->
	 let es = List.map expression es in
	 T.TaggedValues (T.Constructor k, es)

      | S.IfThenElse (a, b, c) ->
	 let a = expression a in
	 let b = expression b in
	 let c = expression c in
	 T.IfThenElse(a, b, c)

      | S.Case (e, sb) ->
	 let e = expression e in
	 let sb = List.map branch sb in
	 T.Case(e, sb)

      | S.Apply (e1, e2) as e ->
	 (match classify_application e with
	  | PrimitiveApplication (S.Id id, l) ->
	     T.(FunCall (FunId id, List.map expression l))
	  | GeneralApplication (e1, e2) ->
	     let e1 = expression e1 in
	     let rec es e = match Position.value e with
	       | S.Apply (e1, e2) -> es e2
	       | _ -> [expression e]
	     in let e2 = es e2 in
	     T.UnknownFunCall (e1, e2)
	 )

      | S.Fun ((env, typ), e) ->
	 let rec descend e = match Position.value e with
	   | S.Fun ((formal, _), e) ->
	      let formals, e = descend e in
	      (identifier formal, tydummy)::formals, e
	   | _ -> [identifier env, tydummy], (expression e)
	 in
	 let x = descend e in
	 let f = push_new_function pos (fst x) (snd x)  in
	 T.(Literal (LFun f))

      | S.RecFuns rfs ->
	 failwith "Student! This is your job!73"
    in
    locate pos (aux e)

      and literal = function
	| S.LInt l ->
	   T.LInt l

      and branch (S.Branch (p, e)) =
	let p = pattern p in
	let e = expression e in
	T.Branch(p, e)

      and pattern p =
	Position.with_pos
	  (Position.position p)
	  (match Position.value p with
	   | S.PWildcard -> T.PWildcard
	   | S.PVariable id -> T.PVariable (identifier id)
	   | S.PTuple ids -> T.PTuple (List.map identifier ids)
	   | S.PTaggedValues (t, ids) -> T.PTaggedValues(tag t, List.map identifier ids)
	  )

      and identifier (S.Id x) = T.Id x
      and tag (S.Constructor x) = T.Constructor x
  in
       program p

       let translate p env =
	 let closed_p = closure_conversion p in
	 (** To see the result of closure conversion: *)
  (* print_endline ("CC: " ^ Hopix.print_ast closed_p ^ "\n"); *)
  (* flush stdout; *)

	 (hoist closed_p, ())

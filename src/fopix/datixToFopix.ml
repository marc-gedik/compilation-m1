(** This module implements a compiler from Datix to Fopix. *)

let error pos msg =
  Error.error "compilation" pos msg

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Datix
module S = Source.AST
module Target = Fopix
module T = Target.AST

type environment = {
  tag_representation   : (S.tag * int) list;
  label_representation : (S.label * int) list;
}

let initial_environment () = {
  tag_representation = [];
  label_representation = [];
}

let bind_tag_representation env m =
  { env with tag_representation = m :: env.tag_representation }

let bind_label_representation env m =
  { env with label_representation = m :: env.label_representation }

let lookup_label_representation env l =
  List.assoc l env.label_representation

let lookup_tag_representation env t =
  List.assoc t env.tag_representation

let fresh_identifier =
  let r = ref 0 in
  fun () -> incr r; T.Id ("_" ^ string_of_int !r)

(** [translate p env] turns a Datix program [p] into a Fopix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) env =

  (** Compilation is done in two steps:

      1. Choose a low-level representation for data and store this
      choice in env.

      2. Use this environment to translate Datix high-level data into
      Fopix blocks.
  *)
  let rec program env p =
    let env = List.fold_left choose_data_representation env p in
    let defs = List.(flatten (map (definition' env) p)) in
    (defs, env)

  and definition' env d =
    definition env (Position.value d)

  and definition env = function
    | S.DefineValue (pat, e) ->
       let id = toplevel_pattern' env e pat in
       id

    | S.DefineFunction (f, xs, _, e) ->
       let f  = function_identifier' f in
       let xs = formals xs in
       let e  = expression' env e in
       [T.DefineFunction (f, xs, e)]

    | S.DefineType (t, tdef) ->
      []


  and expression pos env e =
    let locate = Position.with_pos pos in
    match e with
      | S.Literal l ->
	 T.Literal (literal l)

      | S.Variable (S.Id x) ->
	 T.(Variable (Id x))

      | S.Define (pat, e1, e2) ->
	 let e1 = expression' env e1 in
	 let e2 = expression' env e2 in
	 pattern' env e1 pat e2

      | S.FunCall (S.FunId f, es) ->
	 let es = List.map (expression' env) es in
         T.FunCall (T.FunId f, es)

      | S.IfThenElse (c, et, ef) ->
         let c  = expression' env c  in
	 let et = expression' env et in
	 let ef = expression' env ef in
	 T.IfThenElse (c, et, ef)

      | S.Tuple es ->
	 let id = (fresh_identifier ()) in
	 let vBlock   = T.Variable id in
	 let l = List.mapi (fun i x -> [locate vBlock; locate (create_int i); expression' env x]) es in
	 define pos id "block_create" (create_int_list_args pos [List.length es; 0])
		(List.fold_left (fun x set -> define pos (fresh_identifier ()) "block_set" set x) vBlock l)

      | S.Record rs ->
	 let block = (fresh_identifier ()) in
	 let vBlock   = T.Variable block in
	 let l = List.map (fun (lbl,e) ->
			   [locate vBlock;
			    locate (create_int (lookup_label_representation env lbl));
			    expression' env e])
			  rs
	 in
	 define pos block "block_create" (create_int_list_args pos [List.length rs; 0])
		(List.fold_left (fun x set -> define pos (fresh_identifier ()) "block_set" set x) vBlock l)

      | S.RecordField (e, l) ->
	 let id = fresh_identifier () in
	 let n = lookup_label_representation env l in
         let block = expression' env e in
	 define pos id "block_get" [block; locate (create_int n)] (T.Variable id)

      | S.TaggedValues (k, es) ->
	 let id = fresh_identifier () in
	 let vBlock   = T.Variable id in
	 let n = lookup_tag_representation env k in
	 let size = (List.length es) + 1 in
	 let l = List.mapi (fun i x -> [locate vBlock; locate (create_int (i+1)); expression' env x]) es in
	 define pos id "block_create" (create_int_list_args pos [size;0])
		(define pos (fresh_identifier ()) "block_set" ((locate vBlock)::create_int_list_args pos [0;n])
		(List.fold_left (fun x set -> define pos (fresh_identifier ()) "block_set" set x) vBlock l))

      | S.Case (e, bs) ->
         branches env (expression' env e) bs

  and expression' env e =
    Position.map (expression (Position.position e) env) e

  and branches env x = function
    | [] ->
       create_int 57005
    | S.Branch (pat, e) :: bs ->
       failwith "if branches Student job!"

  and toplevel_pattern pos env x p =
    let locate () = Position.with_pos pos in
    let e = expression' env x in
    match p with
    | S.PWildcard ->
       [T.DefineValue (locate () (T.Id "_"), e)]

    | S.PVariable y ->
       [T.DefineValue (locate () (identifier y), e)]

    | S.PTuple ys ->
       toplevel_block_get pos e ys 0

    | S.PTaggedValues (k, ys) ->
       toplevel_block_get pos e ys 1

  and toplevel_block_get pos e ys n  =
    let locate () = Position.with_pos pos in
    let block = fresh_identifier () in
    let vBlock = locate () (T.Variable block) in
    T.DefineValue (locate () block, e)::
      (List.mapi (fun i x -> defineValue pos (identifier x) "block_get" [vBlock; locate () (create_int (i+n))]) ys)

  and pattern pos env x pat e =
    let locate () = Position.with_pos pos in
    match pat with
      | S.PWildcard ->
	 T.Define (locate () (T.Id "_"), x, e)

      | S.PVariable y ->
	 T.Define (locate ()(identifier y), x, e)

      | S.PTuple ys ->
	 let block = fresh_identifier () in
	 let vBlock = locate () (T.Variable block) in
	 let l = List.mapi (fun i x -> i,x) ys in
	 T.Define (locate () block, x, locate ()
		   (List.fold_left
		      (fun e (i,y) ->
		       define pos (identifier y) "block_get" [vBlock; locate () (create_int i)] e)
		      (Position.value e) l))

      | S.PTaggedValues (k, ys) ->
	 let block = fresh_identifier () in
	 let vBlock = locate () (T.Variable block) in
	 let l = List.mapi (fun i x -> i,x) ys in
	 T.Define (locate () block, x, locate ()
		 (List.fold_left
		      (fun e (i,y) ->
		       define pos (identifier y) "block_get" [vBlock; locate () (create_int (i+1))] e)
		      (Position.value e) l))

  and pattern' env x pat e =
    pattern (Position.position pat) env x (Position.value pat) e

  and toplevel_pattern' env x pat =
    toplevel_pattern (Position.position pat) env x (Position.value pat)

  and literal = function
    | S.LInt x -> T.LInt x

  and identifier (S.Id x) =
    T.Id x

  and function_identifier (S.FunId x) =
    T.FunId x

  and function_identifier' x =
    Position.map function_identifier x

  and formals xs =
    List.(map identifier (fst (split xs)))

  and choose_data_representation env defs =
    match (Position.value defs) with
    | S.DefineType (_, RecordTy l) ->
       let l = List.mapi (fun i (x,_) -> x,i) l in
       List.fold_left (fun env x -> bind_label_representation env x) env l

    | S.DefineType (_, TaggedUnionTy l) ->
       let l = List.mapi (fun i (x,_) -> x,i) l in
       List.fold_left (fun env x -> bind_tag_representation env x) env l

    | _ -> env

  and create_int x =
    T.Literal (T.LInt x)

  and defineValue pos s fid es =
    T.DefineValue (Position.with_pos pos s,
	      Position.with_pos pos (T.FunCall (T.FunId fid, es)))

  and define pos s fid es e =
    T.Define (Position.with_pos pos s,
	      Position.with_pos pos (T.FunCall (T.FunId fid, es)),
	      Position.with_pos pos e)

  and create_int_list_args pos l =
    List.map (fun x -> Position.with_pos pos (create_int x)) l

  in
  program env p

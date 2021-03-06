(** This module implements a type checker for Datix. *)

open HopixAST

(** Type checker error message producer. *)
let error = Error.error "during type checking"

(** Basic types. *)
let tyint  = TyIdentifier (TId "int")
let tybool = TyIdentifier (TId "bool")

(** During typechecking, we thread an environment that contains
    the type of variables and the type definitions. *)
module TypingEnvironment : sig
  (** The type of typing environment. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x ty] returns an environment that assigns [ty]
      the variable [x] and extends [env]. *)
  val bind
      : t -> identifier -> typ -> t

  (** [UnboundIdentifier x] is raised if [x] is unbound. *)
  exception UnboundIdentifier of identifier

  (** [lookup env x] returns the type assigned to [x] in [env].
      Raise {!UnboundIdentifier x} if no such variable exists. *)
  val lookup
      : t -> identifier -> typ

  (** [bind_type_definition env t tdef] introduces a new type definition
      in [env]. *)
  val bind_type_definition
      : t -> type_identifier -> type_definition -> t

  (** [UnboundTypeIdentifier t] is raised if no such type [t] exists. *)
  exception UnboundTypeIdentifier of type_identifier

  (** [lookup_type_definition env t] looks for the definition of [t] in
      [env]. May raise {!UnboundTypeIdentifier t} if no such definition
      exists. *)
  val lookup_type_definition
      : t -> type_identifier -> type_definition

  (** [UnboundLabel l] is raised if no record type contains [l] as a label. *)
  exception UnboundLabel of label

  (** [lookup_recordtype_from_label env l] returns the type identifier
      of the record type that contains [l] as well as its definition.
      May raise {!UnboundLabel l}. *)
  val lookup_recordtype_from_label
      : t -> label -> type_identifier * (label * typ) list

  (** This exception is raised when a type identifier is defined but
      it is not a record type. *)
  exception NotRecordType of type_identifier

  (** [lookup_recordtype env t] returns all the field types of
      the record type [t] in [env].
      May raise {!UnboundTypeIdentifier l} or {!NotRecordType t}.*)
  val lookup_recordtype
      : t -> type_identifier -> (label * typ) list

  (** [UnboundTag t] is raised if no such tag [t] exists. *)
  exception UnboundTag of tag

  (** [lookup_tagged_union_type_from_tag env t] returns the type
      identifier of the union type that contains [t] as a tag as well
      as all the types of the tag parameters.
      May raise {!UnboundTag t}. *)
  val lookup_tagged_union_type_from_tag
      : t -> tag -> type_identifier * (tag * typ list) list

  (** This exception is raised if a type identifier is defined but it
      is not a tagged union. *)
  exception NotTaggedUnion of type_identifier

  (** [lookup_tagged_union_typ env t] returns type definition of
      the tagged union type [t] in [env].
      May raise {!UnboundTypeIdentifier t} or {!NotTaggedUnion t}. *)
  val lookup_tagged_union_type
      : t -> type_identifier -> (tag * typ list) list

end = struct
  type t = {
    variables : (identifier * typ) list;
    typedefs  : (type_identifier * type_definition) list;
  }

  let empty = {
    variables = [];
    typedefs  = [];
  }

  let bind e x ty = { e with
		      variables = (x, ty) :: e.variables
		    }

  exception UnboundIdentifier of identifier

  let lookup e x =
    try
      List.assoc x e.variables
    with Not_found ->
      raise (UnboundIdentifier x)

  let bind_type_definition env t tdef =
    { env with typedefs = (t, tdef) :: env.typedefs }

  exception UnboundTypeIdentifier of type_identifier

  let lookup_type_definition env t =
    try
      List.assoc t env.typedefs
    with Not_found ->
      raise (UnboundTypeIdentifier t)

  exception NotRecordType of type_identifier

  let lookup_recordtype env t =
    match lookup_type_definition env t with
    | RecordTy fs -> fs
    | _ -> raise (NotRecordType t)

  exception UnboundLabel of label

  let lookup_recordtype_from_label env l =
    try
      match List.find (fun (_, tdef) ->
		       match tdef with
		       | RecordTy fs -> List.exists (fun (l', _) -> l' = l) fs
		       | _ -> false
		      ) env.typedefs
      with
      | (i, RecordTy fs) -> (i, fs)
      | _ -> assert false (* Because of the predicate below. *)
    with Not_found ->
      raise (UnboundLabel l)

  exception UnboundTag of tag

  let lookup_tagged_union_type_from_tag env t =
    try
      match List.find (fun (_, tdef) ->
		       match tdef with
		       | TaggedUnionTy ts -> List.exists (fun (t', _) -> t' = t) ts
		       | _ -> false
		      ) env.typedefs
      with
      | (i, TaggedUnionTy ts) -> (i, ts)
      | _ -> assert false (* Because of the predicate below. *)
    with Not_found ->
      raise (UnboundTag t)

  exception NotTaggedUnion of type_identifier

  let lookup_tagged_union_type env t =
    match lookup_type_definition env t with
    | TaggedUnionTy ks -> ks
    | _ -> raise (NotTaggedUnion t)

end

type typing_environment = TypingEnvironment.t

let bind_binop tenv l typ ret =
  List.fold_left
    (fun tenv x -> TypingEnvironment.bind tenv (Id x) (TyArrow (typ, (TyArrow (typ, ret)))))
    tenv l

(** The initial environment contains the type of the primitive functions. *)
let initial_typing_environment () =
  let tenv = TypingEnvironment.empty in
  let tenv = TypingEnvironment.bind_type_definition
	       tenv (TId "bool") (TaggedUnionTy [Constructor "True", []; Constructor "False", []]) in
  let tenv = bind_binop tenv ["+"; "-"; "*"; "/"] tyint tyint in
  let tenv = bind_binop tenv ["<"; "<="; ">"; ">="; "="] tyint tybool in
  tenv

let is_bool c =
  c = tybool || c = (TyIdentifier (TId "bool"))

let check_record_label pos l fs =
  let rec aux l fs =
    match l,fs with
    | [], [] -> ()
    | (lb1,_)::l, (lb2,_)::fs ->
       if lb1 = lb2
       then aux l fs
       else error pos "Unbound record field"
    | [], (lb, _)::l -> error pos "Unbound record field"
    | _, [] ->  error pos "Some record fields are undefined"
  in
  aux l fs

let lambda pos x =
  match x with
  | id, Some typ -> id, typ
  | _, None -> error pos "lambda with no type"

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast =

  let rec program tenv p =
    List.fold_left definition tenv p

  and definition tenv def =
    match Position.value def with
    | DefineValue (p, e) ->
       define_value tenv p e

      | DefineType (t, tdef) ->
        let tenv' = TypingEnvironment.bind_type_definition tenv t (TaggedUnionTy []) in
        well_formed_type_definition (Position.position def) tenv' tdef;
        TypingEnvironment.bind_type_definition tenv t tdef

  and well_formed_type_definition pos tenv = function
    | RecordTy ltys ->
       (* failwith "Student! This is your job!1" *)
       ()

    | TaggedUnionTy ktys ->
       (* failwith "Student! This is your job!2" *)
       ()


  (** [define_value tenv p e] returns a new environment that associates
      a type to each of the variables bound by the pattern [p]. *)
  and define_value tenv p e =
    let ty = infer_expression_type tenv e in
    check_pattern tenv ty p

  (** [infer_expression_type tenv e] returns the type of the expression
      [e] under the environment [tenv] if [e] is well-typed. *)
  and infer_expression_type tenv e =
    let pos = Position.position e in
    match Position.value e with
    | MutateTuple _ ->
       failwith "MutataTuple typechecker"

    | Fun (x, e) ->
       let id, xTyp = lambda pos x in
       let tenv = TypingEnvironment.bind tenv id xTyp in
       let eTyp = infer_expression_type tenv e in
       TyArrow (xTyp, eTyp)

    | RecFuns fs ->
       let xTyp =
	 List.map (fun (x, _) -> lambda (Position.position x) (Position.value x)) fs in
       let tenv =
	 List.fold_left (fun tenv (x,y) -> TypingEnvironment.bind tenv x y) tenv xTyp in
       let _ = List.map2 (fun (_, e) (_,x) -> check_expression_type tenv x e) fs xTyp in
       TyTuple(snd (List.split xTyp))

    | Apply (a, b) ->
       let ta = infer_expression_type tenv a in
       (match ta with
	| TyArrow (typ, ret) -> check_expression_type tenv typ b; ret
	| _ -> error pos "Not applicable"
       )

    | Literal l ->
       infer_literal_type l

    | Variable x ->
       (try
           TypingEnvironment.lookup tenv x
         with TypingEnvironment.UnboundIdentifier (Id x) ->
           error pos (Printf.sprintf "Identifier `%s' is unbound." x)
       )

    | Define (p, e1, e2) ->
       let tenv = define_value tenv p e1 in
       infer_expression_type tenv e2

    | IfThenElse (c, te, fe) ->
       let c = infer_expression_type tenv c in
       let b = is_bool c in
       if b
       then
	 begin
	   let te = infer_expression_type tenv te in
	   let fe = infer_expression_type tenv fe in
	   if te = fe
	   then te
	   else error pos "Not same type"
         end
       else
	 error pos "Not a boolean"

    | Tuple es ->
       let typs = List.map (infer_expression_type tenv) es in
       TyTuple typs

    | Record [] ->
       assert false (* By parsing. *)

    | Record fs ->
       let t,l = TypingEnvironment.lookup_recordtype_from_label tenv (fst (List.hd fs))
       in
       let fs = List.sort compare fs in
       let l  = List.sort compare l  in
       check_record_label pos l fs;
       TyIdentifier t

    | RecordField (e, (Label lid as l)) ->
       let id =
	 match infer_expression_type tenv e with
	 | TyIdentifier id -> id
	 | _ -> error pos "Not a record"
       in
       let typs = TypingEnvironment.lookup_recordtype tenv id in
       List.assoc l typs

    | TaggedValues (k, es) ->
       let id, union = TypingEnvironment.lookup_tagged_union_type_from_tag tenv k in
       let typs = List.assoc k union in
       check_same_length pos es typs;
       List.iter2 (fun e typ -> if (infer_expression_type tenv e) <> typ
				then error pos "Error in args"
				else ())
		  es
		  typs;
       TyIdentifier id

    | Case (e, bs) ->
       let etyp = infer_expression_type tenv e in
       infer_branches tenv etyp None bs



  (** [check_exhaustiveness pos ks bs] ensures that there is no
      forgotten cases in a case analysis assuming that [ks]
      are the only tags that can appear in the patterns of
      branches [bs]. *)
  and check_exhaustiveness pos ks = function
    | [] ->
       failwith "Student! This is your job!16"
    | Branch (pat, _) :: bs ->
       failwith "Student! This is your job!17"

  (** [infer_branches tenv pty previous_branch_type (Branch (p, e))]
          checks that the pattern [p] has type [pty] and that the type of
          [e] (if it exists) is the same as the one of the previous
          branch (unless this is the first branch). *)
  and infer_branches tenv pty previous_branch_type = function
    | [] ->
       begin match previous_branch_type with
             | None -> assert false (* By parsing. *)
             | Some ty -> ty
       end
    | Branch (pat, e) :: bs ->
       let tenv = check_pattern tenv pty pat in
       let etyp = infer_expression_type tenv e in
       begin
	 match previous_branch_type with
         | None -> infer_branches tenv pty (Some etyp) bs
         | Some ty -> if etyp = ty
		      then infer_branches tenv pty (Some ty) bs
		      else error (Position.dummy) "Different type"
       end

  (** [check_pattern tenv pty pat] checks that [pat] can be assigned
      the type [pty] and, if so, returns an extension of [tenv] with
      the variables of [pat] bound to their respective types. *)
  and check_pattern tenv pty pat =
    match Position.value pat, pty with
    | PVariable x, _ ->
       check_variable tenv pty x

    | PTuple xs, TyTuple tys ->
       check_same_length (Position.position pat) xs tys;
       List.fold_left2 check_variable tenv tys xs

    | PWildcard, _ ->
       tenv

    | PTaggedValues (k, xs), TyIdentifier t ->
       let union = TypingEnvironment.lookup_tagged_union_type tenv t in
       let typs  = List.assoc k union in
       check_same_length (Position.position pat) xs typs;
       List.fold_left2 check_variable tenv typs xs

    | _, _ ->
       error (Position.position pat) (
               Printf.sprintf "This pattern has not type: %s\n"
			      (HopixPrettyPrinter.(to_string typ pty))
             )

  and check_irrefutable_pattern tenv pat : unit =
    match (Position.value pat) with
    | PWildcard | PVariable _ | PTuple _ -> ()
    | PTaggedValues (k, _) ->
       let t', ktys = TypingEnvironment.lookup_tagged_union_type_from_tag tenv k in
       if List.length ktys <> 1 then
         error (Position.position pat) "This pattern is not irrefutable."

  and check_variable tenv ty x =
    TypingEnvironment.bind tenv x ty

  and check_expression_type tenv xty e : unit =
    let ity = infer_expression_type tenv e in
    if ity <> xty then
      error (Position.position e) (
              Printf.sprintf "Incompatible types.\nExpected: %s\nInferred: %s\n"
			     (HopixPrettyPrinter.(to_string typ xty))
			     (HopixPrettyPrinter.(to_string typ ity))
	    )

  and check_same_length : type a b. Position.t -> a list -> b list -> unit = fun pos a b ->
			    let aln = List.length a and bln = List.length b in
			    if (aln <> bln) then (
			      error pos
				    (Printf.sprintf
				       "Invalid number of arguments.\nExpected: %d\nGiven: %d\n."
				       aln bln
				    )
			    )

			   and infer_literal_type = function
			     | LInt _ ->
				tyint

			   and check_unicity
			       : type a b. Position.t -> (a * b) list -> string -> unit = fun pos ls what ->
				   let ls = List.(sort (fun (l1, _) (l2, _) -> compare l1 l2) ls) in
				   let ls = fst (List.split ls) in
				   if not (ExtStd.List.all_distinct ls) then
				     error pos (Printf.sprintf "Each %s must appear exactly once." what)

				      in
				      program tenv ast

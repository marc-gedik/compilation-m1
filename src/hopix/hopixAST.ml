(** The abstract syntax tree for hopix programs. *)

open Position

type program = definition located list

and definition =
  | DefineValue of
      pattern located * expression located

  | DefineType of
      type_identifier * type_definition

and expression =
  | Literal of literal
  | Variable of identifier
  | Define of pattern located * expression located * expression located
  | Apply of expression located * expression located
  | Fun of lambda
  | RecFuns of (typed_identifier located * expression located) list
  | Tuple of expression located list
  | Record of (label * expression located) list
  | RecordField of expression located * label
  | TaggedValues of tag * expression located list
  | IfThenElse of expression located * expression located * expression located
  | Case of expression located * branch list

  (* Only appears in the image of closure conversion. *)
  | MutateTuple of expression located * int * expression located

and lambda = typed_identifier * expression located

and typed_identifier = identifier * typ option

and tag =
  | Constructor of string

and branch =
  | Branch of pattern located * expression located

and pattern =
  | PWildcard
  | PVariable     of identifier
  | PTuple        of identifier list
  | PTaggedValues of tag * identifier list

and literal =
  | LInt of int

and identifier =
  | Id of string

and formals =
    typed_identifier list

and typ =
  | TyIdentifier of type_identifier
  | TyTuple      of typ list
  | TyArrow      of typ * typ

and type_definition =
  | RecordTy      of (label * typ) list
  | TaggedUnionTy of (tag * typ list) list

and label =
  | Label of string

and type_identifier =
  | TId of string

and t = program

module VariableSet = Set.Make (struct
    type t = identifier
    let compare (Id s1) (Id s2) = String.compare s1 s2
end)

let seq e1 e2 =
  Position.map (fun _ -> Define (Position.map (fun _ -> PWildcard) e1, e1, e2)) e1

let rec seqs = function
  | [] -> assert false (* By precondition. *)
  | [e] -> e
  | e1 :: e2 -> seq e1 (seqs e2)

let is_binary_primitive = function
  | "+" | "-" | "*" | "/" | "<" | ">" | "<=" | ">=" | "=" -> true
  | _ -> false

let free_variables : expression -> identifier list =
  fun e ->
  let getId id = fst (Position.value id) in
  let fold f = List.fold_left (fun x set -> f set x) in
  let remove_pattern set p =
    match Position.value p with
    | PWildcard -> set
    | PVariable x -> VariableSet.remove x set
    | PTuple xs
    | PTaggedValues (_,xs) ->
       fold VariableSet.remove set xs

     in

  let rec aux' e = aux (Position.value e)

  and aux =
    function
    | Literal _ -> VariableSet.empty

    | Variable (Id id as x) ->
       if is_binary_primitive id then
	 VariableSet.empty
       else
	 VariableSet.singleton x

    | Define (p,e1,e2) -> remove_pattern (VariableSet.union (aux' e1) (aux' e2)) p (* without p *)
    | Apply (e1,e2) -> VariableSet.union (aux' e1) (aux' e2)
    | Fun ((x,_), e) -> VariableSet.remove x (aux' e) (* without x *)

    | RecFuns fs ->
       List.fold_left (fun set (id, e) ->
		       VariableSet.add (getId id) (VariableSet.union (aux' e) set))
		      VariableSet.empty fs

    | Tuple es -> fold VariableSet.union VariableSet.empty (List.map aux' es)

    | Record fs ->
       List.fold_left (fun set (_, e) -> VariableSet.union (aux' e) set) VariableSet.empty fs

    | RecordField (e, lbl) -> aux' e
    | TaggedValues (k, es) -> VariableSet.empty
    | IfThenElse (c, t, f) -> VariableSet.(union (aux' c) (union (aux' t)  (aux' f)))

    | Case (e, bs) ->
       List.fold_left (fun set (Branch(p,e)) ->
		       remove_pattern (VariableSet.union set (aux' e)) p) (aux' e) bs

    (* Only appears in the image of closure conversion. *)
    | MutateTuple (e1, i, e2) -> failwith "Ast.free_variables: MutateTuple"

  in VariableSet.elements (aux e)


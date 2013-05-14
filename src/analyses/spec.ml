(** Analysis by specification file. *)

open Cil
open Pretty
open Analyses
open Batteries

module M = Messages
module SC = SpecCore

module Spec =
struct
  include Analyses.DefaultSpec

  let name = "Spec"
  module Dom  = SpecDomain.Dom
  open Dom.V.T
  module Glob = Glob.Make (Lattice.Unit)

  type glob_fun = Glob.Var.t -> Glob.Val.t

  let return_var = Cil.makeVarinfo false "@return" Cil.voidType
  let stack_var = Cil.makeVarinfo false "@stack" Cil.voidType
  let global_var = Cil.makeVarinfo false "@global" Cil.voidType

  let nodes = ref []
  let edges = ref []

  let load_specfile () =
    let specfile = GobConfig.get_string "spec.file" in
    if String.length specfile < 1 then failwith "You need to specify a specification file using --sets spec.file path/to/file.spec when using the spec analysis!";
    if not (Sys.file_exists specfile) then failwith ("The given spec.file ("^specfile^") doesn't exist (CWD is "^Sys.getcwd ()^").");
    let _nodes, _edges = SpecUtil.parseFile specfile in
    nodes := _nodes; edges := _edges (* don't change -> no need to save them in domain? *)


  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
      | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    (* ignore(printf "%a = %a\n" d_plainlval lval d_plainexp rval); *)
    let m = ctx.local in
    m

  let branch ctx (exp:exp) (tv:bool) : Dom.t =
    (* ignore(printf "if %a = %B (line %i)\n" d_plainexp exp tv (!Tracing.current_loc).line); *)
    ctx.local

  let body ctx (f:fundec) : Dom.t =
    ctx.local

  let callStack m = match Dom.findOption stack_var m with
      | Some(Must(v)) -> v.loc
      | _ -> []

  let callStackStr m = " [call stack: "^(String.concat ", " (List.map (fun x -> string_of_int x.line) (callStack m)))^"]"

  let return ctx (exp:exp option) (f:fundec) : Dom.t =
    let m = ctx.local in
    (* M.write ("return: ctx.local="^(Dom.short 50 ctx.local)^(callStackStr m)); *)
    (* if f.svar.vname <> "main" && BatList.is_empty (callStack m) then M.write ("\n\t!!! call stack is empty for function "^f.svar.vname^" !!!"); *)
    if f.svar.vname = "main" then (
      let warn_main msg_loc msg_end =
        (* find transitions to end state *)
        (* edges that have 'end' as a target *)
        let end_edges = List.filter (fun (a,ws,fwd,b,c) -> b="end") !edges in
        (* we ignore the constraint, TODO maybe find a better syntax for declaring end states *)
        let end_states = List.map (fun (a,ws,fwd,b,c) -> a) end_edges in
        (* used to warn locally or as a summary at the end of main *)
        let warn ?loc:(loc=[!Tracing.current_loc]) maybe msg = M.report ~loc:(List.last loc) ((if maybe then "MAYBE " else "")^msg) in
        (* TODO imperative -> refactor *)
        let must_k = ref [] in
        let may_k = ref [] in
        (* now we check for all entries of our domain if they (maybe) aren't in an end state and warn about it *)
        let check_state k v =
          let must_end = List.exists (fun state -> Dom.in_state k state m) end_states in
          let may_end  = List.exists (fun state -> Dom.may_in_state k state m) end_states in
          let locs = Dom.V.locs v in
          if not may_end then ((* Must: never in an end state *)
            must_k := !must_k@[k.vname];
            match msg_loc with Some msg -> List.iter (fun loc -> warn ~loc:loc false msg) locs | _ -> ()
          )else if not must_end then ((* May: maybe in an end state *)
            may_k := !may_k@[k.vname];
            (* only output MAYBE-warnings for possibilities that are not an end state *)
            (* TODO this is a matter of taste -> make it configurable? *)
            let locs = Dom.V.locs ~p:(fun x -> not (List.mem x.state end_states)) v in
            match msg_loc with Some msg -> List.iter (fun loc -> warn ~loc:loc true msg) locs | _ -> ()
          )
        in
        let no_special_vars = Dom.filter (fun k v -> String.get k.vname 0 <> '@') m in
        Dom.iter check_state no_special_vars;
        match msg_end with
        | Some msg ->
          let f msg ks = Str.global_replace (Str.regexp_string "$") (String.concat ", " ks) msg in
          if not (List.is_empty !must_k) then warn false (f msg !must_k);
          if not (List.is_empty !may_k) then warn true (f msg !may_k)
        | _ -> ()
      in
      (* check if there is a warning for entries that are not in an end state *)
      match SC.warning "_end" !nodes, SC.warning "_END" !nodes with
      | None, None -> () (* nothing to do here *)
      | msg_loc,msg_end -> warn_main msg_loc msg_end
    );
    let au = match exp with
      | Some(Lval(Var(varinfo),offset)) ->
          (* M.write ("return variable "^varinfo.vname^" (dummy: "^return_var.vname^")"); *)
          Dom.add return_var (Dom.find varinfo m) m
      | _ -> m
    in
    (* remove formals and locals *)
    List.fold_left (fun m var -> Dom.remove var m) au (f.sformals @ f.slocals)

  let editStack f m =
    let v = match Dom.findOption stack_var m with
      | Some(Must(v)) -> {v with loc=(f v.loc)}
      | _ -> Dom.V.create stack_var (f []) "" in
    Dom.add stack_var (Must v) m

  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    let m = ctx.local in
    M.debug_each ("entering function "^f.vname^(callStackStr m));
    if f.vname = "main" then load_specfile ();
    let m = if f.vname <> "main" then
      editStack (BatList.cons !Tracing.current_loc) ctx.local
    else m in [m,m]

  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    (* M.write ("leaving function "^f.vname^(callStackStr au)); *)
    let au = editStack List.tl au in
    let return_val = Dom.findOption return_var au in
    match lval, return_val with
      | Some (Var var, offset), Some rval ->
          (* M.write ("setting "^var.vname^" to content of "^(Dom.V.vnames rval)); *)
          let rval = Dom.V.rebind rval var in (* change rval.var to lval *)
          Dom.add var rval (Dom.remove return_var au)
      | _ -> au

  let query_lv ask exp =
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) ->
          Queries.LS.elements l
      | _ -> []

  let rec eval_fv ask (exp:Cil.exp): varinfo option =
    match query_lv ask exp with
      | [(v,_)] -> Some v
      | _ -> None

(*
.spec-format:
- The file contains two types of definitions: nodes and edges. The labels of nodes are output. The labels of edges are the constraints.
- The start node of the first transition is the start node of the automaton.
- Nodes starting with 'w' are warnings, which have an implicit back edge to the previous node.
- Nodes starting with 'f'/'e' (fail/error/exit/end?) are end nodes.
- An edge with '$_' matches everything and forwards it to the target node.
*)
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    (* let _ = GobConfig.set_bool "dbg.debug" false in *)
    let m = ctx.local in
    let ret dom = [dom, Cil.integer 1, true] in
    let dummy = ret ctx.local in
    let loc = !Tracing.current_loc in
    let dloc = loc::(callStack m) in
    let arglist = List.map (Cil.stripCasts) arglist in (* remove casts, TODO safe? *)
    (* get possible varinfos for a given lval *)
    let varinfos lval =
      match lval with (* TODO ignore offset? *)
        | Var varinfo, _ -> [varinfo]
        | Mem exp, _ ->
            let xs = query_lv ctx.ask exp in (* MayPointTo -> LValSet *)
            M.debug_each ("MayPointTo "^(Pretty.sprint 80 (d_exp () exp))^" = ["
              ^(String.concat ", " (List.map (Lval.CilLval.short 80) xs))^"]");
            List.map fst xs
    in
    (* fold possible varinfos on domain *)
    (* let ret_all f lval = ret (List.fold_left f m (varinfos lval)) in *)
    (* custom goto (Dom.goto is just for modifying) that checks if the target state is a warning and acts accordingly *)
    let goto ?may:(may=false) var loc state m ws =
      let warn var m msg =
        let msg = Str.global_replace (Str.regexp_string "$") var.vname msg in
        M.report ((if Dom.is_may var m then "MAYBE " else "")^msg)
      in
      (* do transition warnings *)
      List.iter (fun state -> match SC.warning state !nodes with Some msg -> warn var m msg | _ -> ()) ws;
      match SC.warning state !nodes with
        | Some msg ->
            warn var m msg;
            m (* no goto == implicit back edge *)
        | None ->
            M.debug_each ("GOTO "^var.vname^": "^Dom.get_state var m^" -> "^state);
            if may then Dom.may_goto var loc state m else Dom.goto var loc state m
    in
    let matching m new_a (a,ws,fwd,b,c) =
      (* If we have come to a wildcard, we match it instantly, but since there is no way of determining a key
         this only makes sense if fwd is true (TODO wildcard for global). We pass a state replacement as 'new_a',
         which will be applied in the following checks.
         Multiple forwarding wildcards are not allowed, i.e. new_a must be None, otherwise we end up in a loop. *)
      if SC.is_wildcard c && fwd && new_a=None then Some (m,fwd,Some (b,a)) (* replace b with a in the following checks *)
      else
      (* Assume new_a  *)
      let a = match new_a with
        | Some (x,y) -> if a=x then y else a
        | None -> a
      in
      (* TODO how to detect the key?? use "$foo" as key, "foo" as var in constraint and "_" for anything we're not interested in.
          What to do for multiple keys (e.g. $foo, $bar)? -> Only allow one key & one map per spec-file (e.g. only $ as a key) or implement multiple maps? *)
      (* look inside the constraint if there is a key and if yes, return what it corresponds to *)
      let key =
        match SC.get_key_variant c with
          | `Lval s    ->
            M.debug_each ("Key variant for "^f.vname^": `Lval "^s^". \027[30m "^SC.stmt_to_string c);
            lval
          | `Arg(s, i) ->
            M.debug_each ("Key variant for "^f.vname^": `Arg("^s^", "^string_of_int i^")"^". "^SC.stmt_to_string c); 
            (try
              let arg = List.at arglist i in
              match arg with
                | Lval x -> Some x (* TODO enough to just assume the arg is already there as a Lval? *)
                | _      -> None
            with Invalid_argument s ->
              M.debug_each ("Key out of bounds! Msg: "^s); (* TODO what to do if spec says that there should be more args... *)
              None
            )
          | _          -> None (* `Rval or `None *)
      in
      (* Now we have the key for the map-domain or we don't.
         In case we don't have a key, we have to check the global state. *)
      let key = match key with
        | Some key -> key
        | None -> Cil.var global_var (* creates Var with NoOffset *)
      in
      (* ignore(printf "KEY: %a\n" d_plainlval key); *)
      (* possible varinfos the lval may point to -> TODO use Lval as key for map? But: multiple representations for the same Lval?! *)
      let vars = varinfos key in (* does MayPointTo query for Mem, otherwise [varinfo] *)
      let check_var (m,n) var =
        (* skip transitions we can't take b/c we're not in the right state *)
        (* i.e. if not in map, we must be at the start node or otherwise we must be in one of the possible saved states *)
        if not (Dom.mem var m) && a<>SC.startnode !edges || Dom.mem var m && not (Dom.may_in_state var a m) then (
          (* ignore(printf "SKIP %s: state: %s, a: %s at %i\n" f.vname (Dom.get_state var m) a (!Tracing.current_loc.line)); *)
          (m,n) (* not in map -> initial state. TODO save initial state? *)
        )
        (* skip transitions where the constraint doesn't have the right form (assignment or not) *)
        else if not (SC.equal_form lval c) then (m,n)
        (* check if parameters match those of the constraint (arglist corresponds to args) *)
        else
        let equal_args args =
          if List.length args = 1 && List.hd args = `Free then
            true (* wildcard as an argument matches everything *)
          else if List.length arglist <> List.length args then (
            M.debug_each "SKIP the number of arguments doesn't match the specification!";
            false
          )else
          let equal_arg = function
            (* TODO match constants right away to avoid queries? *)
            | `String a, Const(CStr b) -> M.debug_each ("EQUAL String Const: "^a^" = "^b); a=b
            (* | `String a, Const(CWStr xs as c) -> failwith "not implemented" *)
            (* CWStr is done in base.ml, query only returns `Str if it's safe *)
            | `String a, e -> (match ctx.ask (Queries.EvalStr e) with
                | `Str b -> M.debug_each ("EQUAL String Query: "^a^" = "^b); a=b
                | _      -> M.debug_each "EQUAL String Query: no result!"; false
              )
            | `Regex a, e -> (match ctx.ask (Queries.EvalStr e) with
                | `Str b -> M.debug_each ("EQUAL Regex String Query: "^a^" = "^b); Str.string_match (Str.regexp a) b 0
                | _      -> M.debug_each "EQUAL Regex String Query: no result!"; false
              )
            | `Bool a, e -> (match ctx.ask (Queries.EvalInt e) with
                | `Int b -> (match Queries.ID.to_bool b with Some b -> a=b | None -> false)
                | _      -> M.debug_each "EQUAL Bool Query: no result!"; false
              )
            | `Int a, e  -> (match ctx.ask (Queries.EvalInt e) with
                | `Int b -> (match Queries.ID.to_int b with Some b -> (Int64.of_int a)=b | None -> false)
                | _      -> M.debug_each "EQUAL Int Query: no result!"; false
              )
            | `Float a, Const(CReal (b, fkind, str_opt)) -> a=b
            | `Float a, _ -> M.warn_each "EQUAL Float: unsupported!"; false
            (* arg is a key. currently there can only be one key per constraint, so we already used it for lookup. TODO multiple keys? *)
            | `Vari a, b  -> true
            (* arg is a identifier we use for matching constraints. TODO save in domain *)
            | `Ident a, b -> true
            | `Error s, b -> failwith ("Spec error: "^s)
            (* wildcard matches anything *)
            | `Free, b    -> true
            | a,b -> M.warn_each ("EQUAL? Unmatched case - assume true..."); true
          in List.for_all equal_arg (List.combine args arglist) (* TODO Cil.constFold true arg. Test: Spec and c-file: 1+1 *)
        in
        (* check if arguments match the constraint *)
        if not (equal_args (SC.get_fun_args c)) then (m,n)
        (* everything matches the constraint -> go to new state and increase counter *)
        (* TODO if #Queries.MayPointTo > 1: each result is May, but all combined are Must *)
        else let new_m = goto ~may:(List.length vars > 1) var dloc b m ws in (new_m,n+1)
      in
      (* do check for each varinfo and return the resulting domain if there has been at least one matching constraint *)
      let new_m,n = List.fold_left check_var (m,0) vars in (* start with original domain and #transitions=0 *)
      if n==0 then None (* no constraint matched the current state *)
      else Some (new_m,fwd,None) (* return new domain and forwarding info *)
    in
    (* edges that match the called function name + wildcard transitions, except those for end *)
    let fun_edges = List.filter (fun (a,ws,fwd,b,c) -> SC.fname_is f.vname c || SC.is_wildcard c && fwd && b<>"end") !edges in
    (* go through constraints and return resulting domain for the first match *)
    (* if no constraint matches, the unchanged domain is returned *)
    (* repeat for target node if it is a forwarding edge *)
    (* TODO what should be done if multiple constraints would match? *)
    try
      let rec check_fwd_loop m new_a = (* TODO cycle detection? *)
        let new_m,fwd,new_a = List.find_map (matching m new_a) fun_edges in
        if fwd then check_fwd_loop new_m new_a else new_m
      in ret (check_fwd_loop m None)
    with Not_found -> dummy
(*     match lval, f.vname, arglist with
      | None, "fopen", _ ->
          M.report "file handle is not saved!"; dummy
      | Some lval, "fopen", _ ->
          let f m varinfo =
            (match arglist with
              | Const(CStr(filename))::Const(CStr(mode))::[] ->
                  Dom.fopen varinfo dloc filename mode m
              | e::Const(CStr(mode))::[] ->
                  (* ignore(printf "CIL: %a\n" d_plainexp e); *)
                  (match ctx.ask (Queries.EvalStr e) with
                    | `Str filename -> Dom.fopen varinfo dloc filename mode m
                    | _ -> M.report "no result from query"; m
                  )
              | xs ->
                  M.report (String.concat ", " (List.map (fun x -> Pretty.sprint 80 (d_exp () x)) xs));
                  List.iter (fun exp -> ignore(printf "%a\n" d_plainexp exp)) xs;
                  M.report "fopen needs two strings as arguments"; m
            )
          in ret_all f lval
      | _ -> dummy *)

  let startstate v = Dom.bot ()
  let otherstate v = Dom.bot ()
  let exitstate  v = Dom.bot ()
end

module TransparentSignatureHack: Analyses.Spec = Spec

module ThreadMCP =
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "spec"
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x: lf): MCP.local_state = `Spec x
                let extract_l x = match x with `Spec x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

module Spec2 : Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "spec" (module Spec2 : Spec2)
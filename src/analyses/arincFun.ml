(** Tracking of arinc processes and their actions. Output to console, graphviz and promela. *)

open Batteries
open Cil
open Pretty
open Analyses

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "arincFun"

  (* ARINC types and Hashtables for collecting CFG *)
  type id = varinfo
  type resource = ArincFunUtil.resource
  (* lookup/generate id from resource type and name (needed for LAP_Se_GetXId functions; specified by LAP_Se_CreateX functions during init) *)
  (* map from tuple (resource, name) to varinfo (need to be saved b/c makeGlobalVar x t <> makeGlobalVar x t) *)
  let resources = Hashtbl.create 13
  let get_id (resource,name as k:resource*string) : id =
    try Hashtbl.find resources k
    with Not_found ->
      let vname = ArincFunUtil.str_resource_type resource^":"^name in
      let v = makeGlobalVar vname voidPtrType in
      Hashtbl.replace resources k v;
      v
  let get_by_id (id:id) : (resource*string) option =
    Hashtbl.filter ((=) id) resources |> Hashtbl.keys |> Enum.get
  let get_name_by_id id = get_by_id id |> Option.get |> snd

  (* map process name to integer used in Pid domain *)
  let pnames = Hashtbl.create 13
  let _ = Hashtbl.add pnames "mainfun" 0L
  let get_by_pid pid =
    Hashtbl.filter ((=) pid) pnames |> Hashtbl.keys |> Enum.get
  let get_pid pname =
    try Hashtbl.find pnames pname
    with Not_found ->
      let ids = Hashtbl.values pnames in
      let id = if Enum.is_empty ids then 1L else Int64.succ (Enum.arg_max identity ids) in
      Hashtbl.replace pnames pname id;
      id
  let get_pid_by_id id = get_by_id id |> Option.get |> snd |> get_pid


  (* Domains *)
  include ArincFunDomain

  module G = IntDomain.Booleans
  module C = D

  let sprint f x = Pretty.sprint 80 (f () x)
  let sprint_map f xs = String.concat ", " @@ List.map (sprint f) xs

  let context d = { d with pred = Pred.bot (); ctx = Ctx.bot () }
  (* let val_of d = d *)

  module SymTbl (Unit : sig end) =
  struct
    type t
    let h = Hashtbl.create 123
    let get k =
      try Hashtbl.find h k
      with Not_found ->
        let ids = Hashtbl.values h in
        let id = if Enum.is_empty ids then 0 else (Enum.arg_max identity ids)+1 in
        Hashtbl.replace h k id;
        id
  end
  module CtxTbl = SymTbl (struct end) (* generative functor *)

  let current_ctx_hash () = let hash = !MyCFG.current_ctx_hash |? 0 in string_of_int @@ CtxTbl.get hash
  let current_ctx_short () = !MyCFG.current_ctx_short |? "None"
  let print_current_ctx ?info name f args =
    if name = "foo" then
    let info = match info with Some info -> " ("^info^")" | None -> "" in
    M.debug @@ name^info^": "^f.vname^"("^sprint_map d_exp args ^"), current_ctx_hash = " ^ current_ctx_hash () ^ ", current_ctx_short = " ^ current_ctx_short ()
  let fname_ctx ?ctx f = f.vname ^ "_" ^ (ctx |? current_ctx_hash ())

  let is_single ctx =
    let fl : BaseDomain.Flag.t = snd (Obj.obj (List.assoc "base" ctx.presub)) in
    not (BaseDomain.Flag.is_multi fl)

  let part_mode_var = makeGlobalVar "__GOBLINT_ARINC_MUTLI_THREADED" voidPtrType

  let is_mainfun name = List.mem name (List.map Json.string (GobConfig.get_list "mainfun"))

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let checkPredBot d tf f xs =
    if d.pred = Pred.bot () then M.debug_each @@ tf^": mapping is BOT!!! function: "^f.vname^". "^(String.concat "\n" @@ List.map (fun (n,d) -> n ^ " = " ^ Pretty.sprint 200 (Pred.pretty () d.pred)) xs);
    d

  let body ctx (f:fundec) : D.t = (* enter is not called for spawned processes -> initialize them here *)
    print_current_ctx "body" f.svar [];
    (* M.debug_each @@ "BODY " ^ f.svar.vname ^" @ "^ string_of_int (!Tracing.current_loc).line; *)
    if not (is_single ctx || !Goblintutil.global_initialization || ctx.global part_mode_var) then raise Analyses.Deadcode;
    (* checkPredBot ctx.local "body" f.svar [] *)
    ctx.local

  let last_ctx_hash : int option ref = ref None
  let return ctx (exp:exp option) (f:fundec) : D.t =
    print_current_ctx "return" f.svar [];
    last_ctx_hash := !MyCFG.current_ctx_hash;
    match !MyCFG.current_ctx_hash with
    | Some hash -> { ctx.local with ctx = Ctx.of_int (Int64.of_int hash) }
    | None -> ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list = (* on function calls (also for main); not called for spawned processes *)
    print_current_ctx "enter" f args;
    (* print_endline @@ "ENTER " ^ f.vname ^" @ "^ string_of_int (!Tracing.current_loc).line; (* somehow M.debug_each doesn't print anything here *) *)
    let d_caller = ctx.local in
    let d_callee = { ctx.local with pred = Pred.of_node (MyCFG.Function f) } in (* set predecessor set to start node of function *)
    [d_caller, d_callee]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    let f_ctx_hash = "f_ctx_hash = " ^ string_of_int (!last_ctx_hash |? -1) in
    (* somehow context is only right if f_ctx_hash == current_ctx_hash *)
    print_current_ctx ~info:f_ctx_hash "combine" f args;
    let d_caller = ctx.local in
    let d_callee = au in
    let current_node = Option.get !MyCFG.current_node in
    (* determine if we are in some function or at the root of a process to get the key *)
    let curfun = MyCFG.getFun current_node in
    let curpid = match Pid.to_int ctx.local.pid with Some i -> i | None -> failwith @@ "combine: Pid.to_int = None inside function "^curfun.svar.vname in
    let pname = match get_by_pid curpid with Some s -> s | None -> failwith @@ "combine: no processname for pid in Hashtbl!" in
    let open ArincFunUtil in
    let pfuns = funs_for_process (Process,pname) in
    let pid = if List.exists ((=) curfun.svar) pfuns || is_mainfun curfun.svar.vname then Process, pname else Function, fname_ctx curfun.svar in
    (* check if the callee has some relevant edges, i.e. advanced from the entry point. if not, we generate no edge for the call and keep the predecessors from the caller *)
    if Pred.is_bot d_callee.pred then failwith "d_callee.pred is bot!"; (* set should never be empty *)
    if Pred.equal d_callee.pred (Pred.of_node (MyCFG.Function f)) then
      { d_callee with pred = d_caller.pred }
    else (
      (* write out edges with call to f coming from all predecessor nodes of the caller *)
      (* if Option.is_some !last_ctx_hash && current_ctx_hash () = string_of_int (Option.get !last_ctx_hash) then *)
      if Ctx.is_int d_callee.ctx then (
        let ctx = Ctx.to_int d_callee.ctx |> Option.get |> i64_to_int |> CtxTbl.get |> string_of_int in
        Pred.iter (fun node -> add_edge pid (node, Call (fname_ctx ~ctx:ctx f), current_node)) d_caller.pred
      );
      (* set current node as new predecessor, since something interesting happend during the call *)
      { d_callee with pred = Pred.of_node current_node }
    )

  (* ARINC utility functions *)
  let mode_is_init  i = match Pmo.to_int i with Some 1L | Some 2L -> true | _ -> false
  let mode_is_multi i = Pmo.to_int i = Some 3L
  (* return code data type *)
  type return_code = (* taken from ARINC_653_part1.pdf page 46 *)
  | NO_ERROR       (* request valid and operation performed *)
  | NO_ACTION      (* system’s operational status unaffected by request *)
  | NOT_AVAILABLE  (* the request cannot be performed immediately *)
  | INVALID_PARAM  (* parameter specified in request invalid *)
  | INVALID_CONFIG (* parameter specified in request incompatible with current configuration (e.g., as specified by system integrator) *)
  | INVALID_MODE   (* request incompatible with current mode of operation *)
  | TIMED_OUT      (* time-out associated with request has expired *)
  let int_from_return_code = function
  | NO_ERROR       -> 0
  | NO_ACTION      -> 1
  | NOT_AVAILABLE  -> 2
  | INVALID_PARAM  -> 3
  | INVALID_CONFIG -> 4
  | INVALID_MODE   -> 5
  | TIMED_OUT      -> 6

  (* set of processes to spawn once partition mode is set to NORMAL *)
  let processes = ref []
  let add_process p = processes := List.append !processes [p]

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    let open ArincFunUtil in let _ = 42 in (* sublime's syntax highlighter gets confused without the second let... *)
    let d : D.t = ctx.local in
    let is_arinc_fun = startsWith "LAP_Se_" f.vname in
    let is_creating_fun = startsWith "LAP_Se_Create" f.vname in
    let is_error_handler = false in (* TODO *)
    (* if is_arinc_fun then M.debug_each @@ "d.callstack: " ^ D.string_of_callstack d.callstack; *)
    if M.tracing && is_arinc_fun then (
      let args_str = String.concat ", " (List.map (sprint d_exp) arglist) in
      (* M.tracel "arinc" "found %s(%s)\n" f.vname args_str *)
      M.debug_each @@ "found "^f.vname^"("^args_str^")"
    );
    let curfun = MyCFG.getFun (Option.get !MyCFG.current_node) in (* current_node should always be set here *)
    print_current_ctx ~info:("inside "^curfun.svar.vname) "special" f arglist;
    (* M.debug_each @@ "Inside function "^curfun.svar.vname; *)
    let curpid = match Pid.to_int d.pid with Some i -> i | None -> failwith @@ "special: Pid.to_int = None inside function "^curfun.svar.vname in
    let pname = match get_by_pid curpid with Some s -> s | None -> failwith @@ "special: no processname for pid in Hashtbl!" in
    let curpid = Process, pname in
    let eval_int exp =
      match ctx.ask (Queries.EvalInt exp) with
      | `Int i -> i
      | _ -> failwith @@ "Could not evaluate int-argument "^sprint d_plainexp exp^" in "^f.vname
    in
    let eval_str exp =
      match ctx.ask (Queries.EvalStr exp) with
      | `Str s -> s
      | _ -> failwith @@ "Could not evaluate string-argument "^sprint d_plainexp exp^" in "^f.vname
    in
    let eval_id exp =
      match ctx.ask (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a) ->
                     (* && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) -> *)
          Queries.LS.remove (dummyFunDec.svar, `NoOffset) a |> Queries.LS.elements |> List.map (Option.get%get_by_id%fst)
      | `LvalSet a -> (* failwith "LvalSet was top" *) []
      | x -> M.debug_each @@ "Could not evaluate id-argument "^sprint d_plainexp exp^" in "^f.vname^". Query returned "^sprint Queries.Result.pretty x; []
    in
    let assign_id exp id =
      match exp with
      (* call assign for all analyses (we only need base)! *)
      | AddrOf lval -> ctx.assign ~name:"base" lval (mkAddrOf @@ var id)
      (* TODO not needed for the given code, but we could use Queries.MayPointTo exp in this case *)
      | _ -> failwith @@ "Could not assign id. Expected &id. Found "^sprint d_exp exp
    in
    let assign_id_by_name resource_type name id =
      assign_id id (get_id (resource_type, eval_str name))
    in
    let current_node = Option.get !MyCFG.current_node in
    let add_action action d =
      (* determine parent: Process or Function? *)
      let pfuns = funs_for_process (Process,pname) in
      let pid = if List.exists ((=) curfun.svar) pfuns || is_mainfun curfun.svar.vname then Process, pname else Function, fname_ctx curfun.svar in
      (* add edges for all predecessor nodes (from pred. node to current_node) *)
      Pred.iter (fun node -> ArincFunUtil.add_edge pid (node, action, current_node)) d.pred;
      (* update domain by replacing the set of pred. nodes with the current node *)
      D.pred (const @@ Pred.of_node current_node) d
    in
    let todo () = if false then failwith @@ f.vname^": Not implemented yet!" else d in
    let assume_success exp =
      let f lval = ctx.assign ~name:"base" lval (integer @@ int_from_return_code NO_ERROR) in
      match exp with
      | AddrOf lval -> f lval
      | _ ->
        M.debug_each @@ "assume_success: expected &r. Found "^sprint d_exp exp^". Try to query...";
        match ctx.ask (Queries.MayPointTo exp) with
        | `LvalSet a when not (Queries.LS.is_top a) && Queries.LS.cardinal a = 1 ->
            let lval = Queries.LS.choose a |> fst |> var in
            f lval
        | _ -> failwith @@ "assume_success: could not find out what "^sprint d_exp exp^" may point to..."
    in
    let invalidate_arg exp =
      () (* TODO this is currently done in base b/c there is no interface for invalidating things (maybe use special value for assign or add new function to ctx?) *)
    in
    let arglist = List.map (stripCasts%(constFold false)) arglist in
    if is_arinc_fun && not @@ List.is_empty arglist then (
      let r = List.last arglist in
      if GobConfig.get_bool "ana.arinc.assume_success" then
        assume_success r
      else
        invalidate_arg r
    );
    match f.vname, arglist with
      | _ when is_arinc_fun && is_creating_fun && not(mode_is_init d.pmo) ->
          failwith @@ f.vname^" is only allowed in partition mode COLD_START or WARM_START"
    (* Preemption *)
      | "LAP_Se_LockPreemption", [lock_level; r] when not is_error_handler ->
          add_action LockPreemption d
          |> D.pre (PrE.add (PrE.of_int 1L))
      | "LAP_Se_UnlockPreemption", [lock_level; r] when not is_error_handler ->
          add_action UnlockPreemption d
          |> D.pre (PrE.sub (PrE.of_int 1L))
    (* Partition *)
      | "LAP_Se_SetPartitionMode", [mode; r] -> begin
          match ctx.ask (Queries.EvalInt mode) with
          | `Int i ->
              if M.tracing then M.tracel "arinc" "setting partition mode to %Ld (%s)\n" i (string_of_partition_mode i);
              if mode_is_multi (Pmo.of_int i) then (
                ctx.sideg part_mode_var true;
                (* spawn processes *)
                ignore @@ printf "arinc: SetPartitionMode NORMAL: spawning %i processes!\n" (List.length !processes);
                List.iter (fun (f,f_d) -> ctx.spawn f (f_d d.pre)) !processes; (* what about duplicates? List.unique fails because d is fun! *)
                (* clear list *)
                processes := []
              );
              add_action (SetPartitionMode i) d
              |> D.pmo (const @@ Pmo.of_int i)
          | `Bot -> D.bot ()
          | _ -> ctx.sideg part_mode_var true; D.top ()
          end
      | "LAP_Se_GetPartitionStatus", [status; r] -> todo () (* != mode *)
      | "LAP_Se_GetPartitionStartCondition", [start_condition; r] -> todo ()
    (* treat functions from string.h as extern if they are added at the end of libraryFunctions.ml *)
      | "F59", [dst; src] (* strcpy *)
      | "F60", [dst; src; _] (* strncpy TODO len *)
      | "F61", [dst; src] (* TODO strcpy that ends at \000? *)
      | "F62", [dst; src; _] (* TODO strncpy that ends at \000? *)
      | "F63", [dst; src; _] (* memcpy TODO len *)
      | "F1" , [dst; src; _] (* memset TODO write char src to dst len times  *)
      ->
          (* M.debug @@ "strcpy("^sprint d_plainexp dst^", "^sprint d_plainexp src^")"; *)
          (* let exp = mkAddrOrStartOf (mkMem ~addr:dst ~off:NoOffset) in *)
          let exp = match unrollType (typeOf dst), dst with
            | _, Lval lval
            | _, AddrOf lval
            | _, StartOf lval -> mkAddrOf lval
            | TPtr _, _ -> dst
            | _ -> failwith @@ f.vname ^ " expects first argument to be some Pointer, but got "^sprint d_exp dst^" which is "^sprint d_plainexp dst
          in
          begin match ctx.ask (Queries.MayPointTo exp) with
          | `LvalSet ls when not (Queries.LS.is_top ls) && Queries.LS.cardinal ls = 1 ->
              let v, offs = Queries.LS.choose ls in
              let ciloffs = Lval.CilLval.to_ciloffs offs in
              let lval = Var v, ciloffs in
              (* ignore @@ printf "dst: %a, MayPointTo: %a" d_plainexp dst d_plainlval lval; *)
              ctx.assign ~name:"base" lval src;
              d
          | r ->
              M.debug_each @@ f.vname ^ ": result of MayPointTo for " ^ sprint d_plainexp exp ^ " = " ^ Queries.Result.short 100 r;
              begin match r with
              | `LvalSet ls when Queries.LS.is_top ls -> d
              | `Top -> d
              | _ -> failwith @@ f.vname ^ " could not query MayPointTo "^sprint d_plainexp exp
              end
          end
    (* Processes *)
      | "LAP_Se_CreateProcess", [AddrOf attr; pid; r] ->
          let cm = match unrollType (typeOfLval attr) with
            | TComp (c,_) -> c
            | _ -> failwith "type-error: first argument of LAP_Se_CreateProcess not a struct."
          in
          let struct_fail x =
            failwith @@ "LAP_Se_CreateProcess: problem with first argument: " ^
            begin match x with
            | `Field ofs -> "cannot access field " ^ ofs
            | `Result (name, entry_point, pri, per, cap) ->
              "struct PROCESS_ATTRIBUTE_TYPE needs all of the following fields (with result): NAME ("^name^"), ENTRY_POINT ("^entry_point^"), BASE_PRIORITY ("^pri^"), PERIOD ("^per^"), TIME_CAPACITY ("^cap^")"
            end ^ "\nRunning scrambled: "^string_of_bool Goblintutil.scrambled
          in
          let field ofs =
            try Lval (addOffsetLval (Field (getCompField cm ofs, NoOffset)) attr)
            with Not_found -> struct_fail (`Field ofs)
          in
          let name = ctx.ask (Queries.EvalStr (field Goblintutil.arinc_name)) in
          let entry_point = ctx.ask (Queries.ReachableFrom (AddrOf attr)) in
          let pri  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_base_priority)) in
          let per  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_period)) in
          let cap  = ctx.ask (Queries.EvalInt (field Goblintutil.arinc_time_capacity)) in
          begin match name, entry_point, pri, per, cap with
          | `Str name, `LvalSet ls, `Int pri, `Int per, `Int cap when not (Queries.LS.is_top ls)
            && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
              let funs_ls = Queries.LS.filter (fun l -> isFunctionType (fst l).vtype) ls in
              if M.tracing then M.tracel "arinc" "starting a thread %a with priority '%Ld' \n" Queries.LS.pretty funs_ls pri;
              let funs = funs_ls |> Queries.LS.elements |> List.map fst |> List.unique in
              let spawn f =
                let f_d pre = { pid = Pid.of_int (get_pid name); pri = Pri.of_int pri; per = Per.of_int per; cap = Cap.of_int cap; pmo = Pmo.of_int 3L; pre = pre; pred = Pred.of_node (MyCFG.Function f); ctx = Ctx.bot () } in (* int64 -> D.t *)
                add_process (f,f_d)
              in
              List.iter spawn funs;
              let pid' = Process, name in
              assign_id pid (get_id pid');
              add_action (CreateProcess Action.({ pid = pid'; funs; pri; per; cap })) d
          (* TODO when is `Bot returned? *)
          (* | `Bot, _ | _, `Bot -> D.bot () *)
          | _ -> let f = Queries.Result.short 30 in struct_fail (`Result (f name, f entry_point, f pri, f per, f cap))
          end
      | "LAP_Se_GetProcessId", [name; pid; r] ->
          assign_id_by_name Process name pid; d
      | "LAP_Se_GetProcessStatus", [pid; status; r] -> todo ()
      | "LAP_Se_GetMyId", [pid; r] ->
          assign_id pid (get_id (Process,pname)); d
      | "LAP_Se_Start", [pid; r] ->
          (* at least one process should be started in main *)
          let pid = eval_id pid in
          if List.is_empty pid then d else add_action (Start pid) d
      | "LAP_Se_DelayedStart", [pid; delay; r] -> todo ()
      | "LAP_Se_Stop", [pid; r] ->
          let pid = eval_id pid in
          if List.is_empty pid then d else add_action (Stop pid) d
      | "LAP_Se_StopSelf", [] ->
          add_action (Stop [curpid]) d
      | "LAP_Se_Suspend", [pid; r] ->
          let pid = eval_id pid in
          if List.is_empty pid then d else add_action (Suspend pid) d
      | "LAP_Se_SuspendSelf", [timeout; r] -> (* TODO timeout *)
          add_action (Suspend [curpid]) d
      | "LAP_Se_Resume", [pid; r] ->
          let pid = eval_id pid in
          if List.is_empty pid then d else add_action (Resume pid) d
    (* Logbook *)
      | "LAP_Se_CreateLogBook", [name; max_size; max_logged; max_in_progress; lbid; r] -> todo ()
      | "LAP_Se_ReadLogBook", _ -> todo ()
      | "LAP_Se_WriteLogBook", _ -> todo ()
      | "LAP_Se_ClearLogBook", _ -> todo ()
      | "LAP_Se_GetLogBookId", _ -> todo ()
      | "LAP_Se_GetLogBookStatus", _ -> todo ()
    (* SamplingPort *)
      | "LAP_Se_CreateSamplingPort", [name; max_size; dir; period; spid; r] -> todo ()
      | "LAP_Se_WriteSamplingMessage", _ -> todo ()
      | "LAP_Se_ReadSamplingMessage", _ -> todo ()
      | "LAP_Se_GetSamplingPortId", _ -> todo ()
      | "LAP_Se_GetSamplingPortStatus", _ -> todo ()
    (* QueuingPort *)
      | "LAP_Se_CreateQueuingPort", [name; max_size; max_range; dir; queuing; qpid; r] -> todo ()
      | "LAP_Se_SendQueuingMessage", _ -> todo ()
      | "LAP_Se_ReceiveQueuingMessage", _ -> todo ()
      | "LAP_Se_GetQueuingPortId", _ -> todo ()
      | "LAP_Se_GetQueuingPortStatus", _ -> todo ()
    (* Buffer *)
      | "LAP_Se_CreateBuffer", [name; max_size; max_range; queuing; buid; r] -> todo ()
      | "LAP_Se_SendBuffer", _ -> todo ()
      | "LAP_Se_ReceiveBuffer", _ -> todo ()
      | "LAP_Se_GetBufferId", _ -> todo ()
      | "LAP_Se_GetBufferStatus", _ -> todo ()
    (* Blackboard *)
      | "LAP_Se_CreateBlackboard", [name; max_size; bbid; r] ->
          let bbid' = Blackboard, eval_str name in
          assign_id bbid (get_id bbid');
          add_action (CreateBlackboard bbid') d
      | "LAP_Se_DisplayBlackboard", [bbid; msg_addr; len; r] ->
          let id = eval_id bbid in
          if List.is_empty id then d else add_action (DisplayBlackboard id) d
      | "LAP_Se_ReadBlackboard", [bbid; timeout; msg_addr; len; r] ->
          let id = eval_id bbid in
          if List.is_empty id then d else add_action (ReadBlackboard (id, eval_int timeout)) d
      | "LAP_Se_ClearBlackboard", [bbid; r] ->
          let id = eval_id bbid in
          if List.is_empty id then d else add_action (ClearBlackboard (id)) d
      | "LAP_Se_GetBlackboardId", [name; bbid; r] ->
          assign_id_by_name Blackboard name bbid; d
      | "LAP_Se_GetBlackboardStatus", _ -> todo ()
    (* Semaphores *)
      | "LAP_Se_CreateSemaphore", [name; cur; max; queuing; sid; r] ->
          (* create resource for name *)
          let sid' = Semaphore, eval_str name in
          assign_id sid (get_id sid');
          add_action (CreateSemaphore Action.({ sid = sid'; cur = eval_int cur; max = eval_int max; queuing = eval_int queuing })) d
      | "LAP_Se_WaitSemaphore", [sid; timeout; r] -> (* TODO timeout *)
          let sid = eval_id sid in
          if List.is_empty sid then d else add_action (WaitSemaphore sid) d
      | "LAP_Se_SignalSemaphore", [sid; r] ->
          let sid = eval_id sid in
          if List.is_empty sid then d else add_action (SignalSemaphore sid) d
      | "LAP_Se_GetSemaphoreId", [name; sid; r] ->
          assign_id_by_name Semaphore name sid; d
      | "LAP_Se_GetSemaphoreStatus", [sid; status; r] -> todo ()
    (* Events (down after create/reset, up after set) *)
      | "LAP_Se_CreateEvent", [name; eid; r] ->
          let eid' = Event, eval_str name in
          assign_id eid (get_id eid');
          add_action (CreateEvent eid') d
      | "LAP_Se_SetEvent", [eid; r] ->
          let eid = eval_id eid in
          if List.is_empty eid then d else add_action (SetEvent eid) d
      | "LAP_Se_ResetEvent", [eid; r] ->
          let eid = eval_id eid in
          if List.is_empty eid then d else add_action (ResetEvent eid) d
      | "LAP_Se_WaitEvent", [eid; timeout; r] -> (* TODO timeout *)
          let eid = eval_id eid in
          if List.is_empty eid then d else add_action (WaitEvent (eid, eval_int timeout)) d
      | "LAP_Se_GetEventId", [name; eid; r] ->
          assign_id_by_name Event name eid; d
      | "LAP_Se_GetEventStatus", [eid; status; r] -> todo ()
    (* Time *)
      | "LAP_Se_GetTime", [time; r] -> todo ()
      | "LAP_Se_TimedWait", [delay; r] ->
          add_action (TimedWait (eval_int delay)) d
      | "LAP_Se_PeriodicWait", [r] ->
          add_action PeriodicWait d
    (* Errors *)
      | "LAP_Se_CreateErrorHandler", [entry_point; stack_size; r] ->
          begin match ctx.ask (Queries.ReachableFrom (entry_point)) with
          | `LvalSet ls when not (Queries.LS.is_top ls) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) ls) ->
              let name = "ErrorHandler" in
              let funs = Queries.LS.filter (fun l -> isFunctionType (fst l).vtype) ls |> Queries.LS.elements |> List.map fst |> List.unique in
              let spawn f =
                let f_d pre = { pid = Pid.of_int (get_pid name); pri = Pri.of_int infinity; per = Per.of_int infinity; cap = Cap.of_int infinity; pmo = Pmo.of_int 3L; pre = pre; pred = Pred.of_node (MyCFG.Function f); ctx = Ctx.bot () } in (* int64 -> D.t *)
                add_process (f,f_d)
              in
              List.iter spawn funs;
              add_action (CreateErrorHandler ((Process, name), funs)) d
          | _ -> failwith @@ "CreateErrorHandler: could not find out which functions are reachable from first argument!"
          end
      | "LAP_Se_GetErrorStatus", [status; r] -> todo ()
      | "LAP_Se_RaiseApplicationError", [error_code; message_addr; length; r] -> todo ()
    (* Not allowed: change configured schedule *)
      | "LAP_Se_SetPriority", [pid; prio; r] -> todo ()
      | "LAP_Se_Replenish", [budget; r] -> todo () (* name used in docs *)
      | "LAP_Se_ReplenishAperiodic", [budget; r] -> todo () (* name used in stdapi.c *)
      | _ when is_arinc_fun -> failwith @@ "Function "^f.vname^" not handled!"
      | _ -> d

  let query ctx (q:Queries.t) : Queries.Result.t =
    let d = ctx.local in
    match q with
      | Queries.Priority _ ->
          if Pri.is_int d.pri then
            `Int (Option.get @@ Pri.to_int d.pri)
          else if Pri.is_top d.pri then `Top else `Bot
      | Queries.IsPublic _ ->
          `Bool (not((PrE.to_int d.pre <> Some 0L && PrE.to_int d.pre <> None) || mode_is_init d.pmo))
      | _ -> Queries.Result.top ()

  let finalize () =
    ArincFunUtil.print_actions ();
    if Sys.file_exists "result" then ArincFunUtil.marshal @@ open_out_bin @@ "result/arinc.cs" ^ string_of_int (GobConfig.get_int "ana.arinc.cs_len") ^ ".out";
    if GobConfig.get_bool "ana.arinc.export" then (
      ArincFunUtil.save_dot_graph ();
      ArincFunUtil.save_promela_model ()
    )

  let startstate v = { (D.bot ()) with  pid = Pid.of_int 0L; pmo = Pmo.of_int 1L; pre = PrE.of_int 0L; pred = Pred.of_node (MyCFG.Function (emptyFunction "main").svar) }
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

let _ =
  MCP.register_analysis ~dep:["base"] (module Spec : Spec)

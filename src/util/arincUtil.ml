open Batteries
open Cil
module M  = Messages

(* ARINC types and Hashtables for collecting CFG *)
type resource = Process | Semaphore | Event | Logbook | SamplingPort | QueuingPort | Buffer | Blackboard
let str_resource_type = function
  | Process -> "Process"
  | Semaphore -> "Semaphore"
  | Event -> "Event"
  | Logbook -> "Logbook"
  | SamplingPort -> "SamplingPort"
  | QueuingPort -> "QueuingPort"
  | Buffer -> "Buffer"
  | Blackboard -> "Blackboard"
(* id is resource type and name, there is a 1:1 mapping to varinfo in the analysis uses for assignments *)
type id = resource*string
type ids = id list
type time = int64 (* Maybe use Nativeint which is the same as C long. OCaml int is just 31 or 63 bits wide! *)
module Action = (* encapsulate types because some process field names are also used for D.t -> use local opening of modules (since OCaml 4.00) for output *)
struct
  type process = { pid: id; funs: varinfo list; pri: int64; per: time; cap: time }
  type semaphore = { sid: id; cur: int64; max: int64; queuing: int64 }
end
type action =
  | LockPreemption | UnlockPreemption | SetPartitionMode of int64
  | CreateProcess of Action.process | CreateErrorHandler of id * varinfo list | Start of ids | Stop of ids | Suspend of ids | Resume of ids
  | CreateBlackboard of id | DisplayBlackboard of ids | ReadBlackboard of ids * time | ClearBlackboard of ids
  | CreateSemaphore of Action.semaphore | WaitSemaphore of ids | SignalSemaphore of ids
  | CreateEvent of id | WaitEvent of ids * time | SetEvent of ids | ResetEvent of ids
  | TimedWait of time | PeriodicWait
(* callstack for locations *)
type callstack = location list
type node = MyCFG.node * callstack
let string_of_node = ArincDomain.Pred.NodeSet.string_of_elt
type edge = node * action * node
let action_of_edge (_, action, _) = action
type edges = (id, edge Set.t) Hashtbl.t
let edges = ref @@ Hashtbl.create 199

let marshal ch = Marshal.to_channel ch !edges []
let unmarshal ch = edges := Marshal.from_channel ch

let get_edges (pid:id) : edge Set.t =
  Hashtbl.find_default !edges pid Set.empty
let add_edge (pid:id) edge =
  Hashtbl.modify_def Set.empty pid (Set.add edge) !edges

let filter_map_actions p =
  let all_edges = Hashtbl.values !edges |> List.of_enum |> List.map Set.elements |> List.concat in
  List.filter_map (p%action_of_edge) all_edges

let filter_actions p =
  (* filter_map_actions (Option.filter p % Option.some) *)
  filter_map_actions (fun x -> if p x then Some x else None)

let funs_for_process id : varinfo list =
  let get_funs = function
    | CreateProcess x when x.Action.pid=id -> Some x.Action.funs
    | CreateErrorHandler (id', funs) when id'=id -> Some funs
    | _ -> None
  in
  filter_map_actions get_funs |> List.concat |> List.unique

(* constants and helpers *)
let infinity = 4294967295L (* time value used for infinity *)
let string_of_partition_mode = function
  | 0L -> "IDLE"
  | 1L -> "COLD_START"
  | 2L -> "WARM_START"
  | 3L -> "NORMAL"
  | _  -> "UNKNOWN!"
let string_of_queuing_discipline = function
  | 0L -> "FIFO"
  | 1L -> "PRIO"
  | _  -> "UNKNOWN!"

(* ARINC output *)
(* common *)
let str_i64 id = string_of_int (i64_to_int id)
let str_time t = if t = infinity then "∞" else str_i64 t^"ns"
(* console and dot *)
let str_funs funs = "["^(List.map (fun v -> v.vname) funs |> String.concat ", ")^"]"
let str_resource id =
  match id with
  | Process, "mainfun" ->
      "mainfun/["^String.concat ", " (List.map Json.string (GobConfig.get_list "mainfun"))^"]"
  | Process, name ->
      name^"/"^str_funs @@ funs_for_process id
  | resource_type, name ->
      name
let str_resources ids = "["^(String.concat ", " @@ List.map str_resource ids)^"]"
let str_action pid = function
  | LockPreemption -> "LockPreemption"
  | UnlockPreemption -> "UnlockPreemption"
  | SetPartitionMode i -> "SetPartitionMode "^string_of_partition_mode i
  | CreateProcess x ->
      let open Action in "CreateProcess "^str_resource x.pid^" (funs "^str_funs x.funs^", prio "^str_i64 x.pri^", period "^str_time x.per^", capacity "^str_time x.cap^")"
  | CreateErrorHandler (id, funs) -> "CreateErrorHandler "^str_resource id
  | Start ids -> "Start "^str_resources ids
  | Stop ids when ids=[pid] -> "StopSelf"
  | Stop ids -> "Stop "^str_resources ids
  | Suspend ids when ids=[pid] -> "SuspendSelf"
  | Suspend ids -> "Suspend "^str_resources ids
  | Resume ids -> "Resume "^str_resources ids
  | CreateBlackboard id -> "CreateBlackboard "^str_resource id
  | DisplayBlackboard ids -> "DisplayBlackboard "^str_resources ids
  | ReadBlackboard (ids, timeout) -> "ReadBlackboard "^str_resources ids^" (timeout "^str_time timeout^")"
  | ClearBlackboard ids -> "ClearBlackboard "^str_resources ids
  | CreateSemaphore x ->
      let open Action in "CreateSemaphore "^str_resource x.sid^" ("^str_i64 x.cur^"/"^str_i64 x.max^", "^string_of_queuing_discipline x.queuing^")"
  | WaitSemaphore ids -> "WaitSemaphore "^str_resources ids
  | SignalSemaphore ids -> "SignalSemaphore "^str_resources ids
  | CreateEvent id -> "CreateEvent "^str_resource id
  | WaitEvent (ids, timeout) -> "WaitEvent "^str_resources ids^" (timeout "^str_time timeout^")"
  | SetEvent ids -> "SetEvent "^str_resources ids
  | ResetEvent ids -> "ResetEvent "^str_resources ids
  | TimedWait t -> "TimedWait "^str_time t
  | PeriodicWait -> "PeriodicWait"
(* spin/promela *)
let pml_resources = Hashtbl.create 13
let _ = Hashtbl.add pml_resources (Process, "mainfun") 0L
let id_pml id = (* give ids starting from 0 (get_pid_by_id for all resources) *)
  let resource, name as k = id in
  try Hashtbl.find pml_resources k
  with Not_found ->
    let ids = Hashtbl.filteri (fun (r,n) v -> r=resource) pml_resources |> Hashtbl.values in
    let id = if Enum.is_empty ids then 0L else Int64.succ (Enum.arg_max identity ids) in
    Hashtbl.replace pml_resources k id;
    id
let str_id_pml id = str_i64 @@ id_pml id
let str_ids_pml ids f = String.concat " " (List.map (f%str_id_pml) ids)
let str_action_pml pid = function
  | LockPreemption -> "LockPreemption();"
  | UnlockPreemption -> "UnlockPreemption();"
  | SetPartitionMode i -> "SetPartitionMode("^string_of_partition_mode i^");"
  | CreateProcess x ->
      let open Action in
      "CreateProcess("^str_id_pml x.pid^", "^str_i64 x.pri^", "^str_i64 x.per^", "^str_i64 x.cap^"); // "^str_resource x.pid^" (prio "^str_i64 x.pri^", period "^str_time x.per^", capacity "^str_time x.cap^")\n"
  | CreateErrorHandler (id, funs) -> "CreateErrorHandler("^str_id_pml id^");"
  | Start ids -> str_ids_pml ids (fun id -> "Start("^id^");")
  | Stop ids -> str_ids_pml ids (fun id -> "Stop("^id^");")
  | Suspend ids -> str_ids_pml ids (fun id -> "Suspend("^id^");")
  | Resume ids -> str_ids_pml ids (fun id -> "Resume("^id^");")
  | CreateBlackboard id -> "CreateBlackboard("^str_id_pml id^");"
  | DisplayBlackboard ids -> str_ids_pml ids (fun id -> "DisplayBlackboard("^id^");")
  | ReadBlackboard (ids, timeout) -> str_ids_pml ids (fun id -> "ReadBlackboard("^id^");")
  | ClearBlackboard ids -> str_ids_pml ids (fun id -> "ClearBlackboard("^id^");")
  | CreateSemaphore x ->
      let open Action in
      "CreateSemaphore("^str_id_pml x.sid^", "^str_i64 x.cur^", "^str_i64 x.max^", "^string_of_queuing_discipline x.queuing^");"
  | WaitSemaphore ids -> str_ids_pml ids (fun id -> "WaitSemaphore("^id^");")
  | SignalSemaphore ids -> str_ids_pml ids (fun id -> "SignalSemaphore("^id^");")
  | CreateEvent id -> "CreateEvent("^str_id_pml id^");"
  | WaitEvent (ids, timeout) -> str_ids_pml ids (fun id -> "WaitEvent("^id^");")
  | SetEvent ids -> str_ids_pml ids (fun id -> "SetEvent("^id^");")
  | ResetEvent ids -> str_ids_pml ids (fun id -> "ResetEvent("^id^");")
  | TimedWait t -> "TimedWait("^str_i64 t^");"
  | PeriodicWait -> "PeriodicWait();"
let print_actions () =
  let print_process pid =
    let str_node = string_of_node in
    let str_edge (a, action, b) = str_node a ^ " -> " ^ str_action pid action ^ " -> " ^ str_node b in
    let xs = Set.map str_edge (get_edges pid) in
    M.debug @@ str_resource pid^" ->\n\t"^String.concat "\n\t" (Set.elements xs)
  in
  Hashtbl.keys !edges |> Enum.iter print_process
let save_result desc ext content = (* output helper *)
  let dir = Goblintutil.create_dir "result" in (* returns abs. path *)
  let path = dir ^ "/arinc.cs" ^ string_of_int (GobConfig.get_int "ana.arinc.cs_len") ^ "." ^ ext in
  output_file path content;
  print_endline @@ "saved " ^ desc ^ " as " ^ path
let save_dot_graph () =
  let dot_process pid =
    (* 1 -> w1 [label="fopen(_)"]; *)
    let str_node x = "\"" ^ str_id_pml pid ^ "_" ^ string_of_node x ^ "\"" in (* quote node names for dot b/c of callstack *)
    let str_edge (a, action, b) = str_node a ^ "\t->\t" ^ str_node b ^ "\t[label=\"" ^ str_action pid action ^ "\"]" in
    let xs = Set.map str_edge (get_edges pid) |> Set.elements in
    ("subgraph \"cluster_"^str_resource pid^"\" {") :: xs @ ("label = \""^str_resource pid^"\";") :: ["}\n"]
  in
  let lines = Hashtbl.keys !edges |> List.of_enum |> List.map dot_process |> List.concat in
  let dot_graph = String.concat "\n  " ("digraph file {"::lines) ^ "\n}" in
  save_result "graph" "dot" dot_graph
let save_promela_model () =
  let open Action in (* needed to distinguish the record field names from the ones of D.t *)
  let comp2 f g a b = f (g a) (g b) in (* why is this not in batteries? *)
  let compareBy f = comp2 compare f in
  let find_option p xs = try Some (List.find p xs) with Not_found -> None in (* why is this in batteries for Hashtbl but not for List? *)
  let flat_map f = List.flatten % List.map f in (* and this? *)
  let indent s = "\t"^s in
  let procs  = List.unique @@ filter_map_actions (function CreateProcess x -> Some x | _ -> None) in
  let has_error_handler = not @@ List.is_empty @@ filter_actions (function CreateErrorHandler _ -> true | _ -> false) in
  let semas  = List.unique @@ filter_map_actions (function CreateSemaphore x -> Some x | _ -> None) in
  let events = List.unique @@ filter_map_actions (function CreateEvent id -> Some id | _ -> None) in
  let nproc  = List.length procs + 1 + (if has_error_handler then 1 else 0) in (* +1 is init process *)
  let nsema  = List.length semas in
  let nevent = List.length events in
  let run_processes = List.map (fun x -> let name = snd x.pid in let id = id_pml x.pid in id, "run "^name^"("^str_i64 id^");") procs |> List.sort (compareBy fst) |> List.map snd in
  let init_body =
    "preInit;" ::
    "run mainfun(0);" :: (* keep mainfun as name for init process? *)
    "postInit();" ::
    "run monitor();" ::
    (if has_error_handler then "run ErrorHandler("^str_id_pml (Process, "ErrorHandler")^")" else "") ::
    run_processes
  in
  let process_def id =
    let pid = id_pml id in
    let pname = snd id in
    let proc = find_option (fun x -> x.pid=id) procs in (* None for mainfun *)
    (* build adjacency matrix for all nodes of this process *)
    let module HashtblN = Hashtbl.Make (ArincDomain.Pred.NodeSet.Base) in
    let module SetN = Set.Make (ArincDomain.Pred.NodeSet.Base) in
    let a2bs = HashtblN.create 97 in
    Set.iter (fun (a, _, b as edge) -> HashtblN.modify_def Set.empty a (Set.add edge) a2bs) (get_edges id);
    let nodes = HashtblN.keys a2bs |> List.of_enum in
    (* let get_a (a,_,_) = a in *)
    let get_b (_,_,b) = b in
    (* let out_edges node = HashtblN.find_default a2bs node Set.empty |> Set.elements in (* Set.empty leads to Out_of_memory!? *) *)
    let out_edges node = try HashtblN.find a2bs node |> Set.elements with Not_found -> [] in
    let in_edges node = HashtblN.filter (Set.mem node % Set.map get_b) a2bs |> HashtblN.values |> List.of_enum |> flat_map Set.elements in
    let start_node = List.find (List.is_empty % in_edges) nodes in (* node with no incoming edges is the start node *)
    (* let str_nodes xs = "{"^(List.map string_of_node xs |> String.concat ",")^"}" in *)
    let str_callstack xs = if List.is_empty xs then "" else "__"^String.concat "_" (List.map (fun loc -> string_of_int loc.line) xs) in
    let label ?prefix:(prefix="P") (n,cs) = let node_str = if (MyCFG.getLoc n).line = -1 then "0" else ArincDomain.Pred.NodeSet.Base.string_of_node n in prefix ^ str_i64 pid ^ "_" ^ node_str ^ str_callstack cs in
    let end_label = "P" ^ str_i64 pid ^ "_end" in
    let goto node = "goto " ^ label node in
    let str_edge (a, action, b) = let target = if List.is_empty (out_edges b) then "goto "^end_label else goto b in str_action_pml id action ^ " " ^ target in
    let choice xs = List.map (fun x -> "::\t"^x ) xs in (* choices in if-statements are prefixed with :: *)
    let walk_edges (a, out_edges) =
      let edges = Set.elements out_edges |> List.map str_edge in
      (label a ^ ":") ::
      if List.length edges > 1 then
        "if" :: (choice edges) @ ["fi"]
      else
        edges
    in
    let body = goto start_node :: (flat_map walk_edges (HashtblN.enum a2bs |> List.of_enum)) @ [end_label ^ ":"] in
    let priority = match proc with Some proc -> " priority "^str_i64 proc.pri | _ -> "" in
    "" :: ("proctype "^pname^"(byte id)"^priority^" provided canRun("^str_i64 pid^") {") ::
    List.map indent body @ ["}"]
  in
  let process_defs = Hashtbl.keys !edges |> List.of_enum |> List.sort (compareBy id_pml) |> List.map process_def |> List.concat in
  let promela = String.concat "\n" @@
    ("#define nproc "^string_of_int nproc) ::
    ("#define nsema "^string_of_int nsema) ::
    ("#define nevent "^string_of_int nevent) :: "" ::
    "#include \"arinc_base.pml\"" :: "" ::
    "init {" :: List.map indent init_body @ "}" ::
    process_defs
  in
  save_result "promela model" "pml" promela;
  print_endline ("Copy spin/arinc_base.pml to same folder and then do: spin -a arinc.pml && cc -o pan pan.c && ./pan")

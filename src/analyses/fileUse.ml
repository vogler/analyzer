open Cil
open Pretty
open Analyses

module M = Messages


module Spec =
struct
  include Analyses.DefaultSpec

  let name = "File Use"
  module Dom  = FileDomain.FileUses
  open Dom.V.T
  module Glob = Glob.Make (Lattice.Unit)

  type glob_fun = Glob.Var.t -> Glob.Val.t

  (* let return_var = dummyFunDec.svar (* see base.ml: 219 *) *)
  let return_var = Cil.makeVarinfo false "@return" Cil.voidType
  let stack_var = Cil.makeVarinfo false "@stack" Cil.voidType

  let lval2var (lhost,offset) =
    match lhost with
      | Var varinfo -> varinfo
      | Mem exp -> M.bailwith "lval not var"

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
      (* | Queries.MayEscape v -> `Bool (Dom.mem v ctx.local) *)
      | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    (* let _ = printf "%a = %a\n" (printLval plainCilPrinter) lval (printExp plainCilPrinter) rval in *)
    (* TODO: test 15 *)
    let m = ctx.local in
    let var = lval2var lval in
    if Dom.mem var m then (
      M.report ("changed file pointer "^var.vname^" (no longer safe)");
      Dom.may var m
    )else
      m

  let branch ctx (exp:exp) (tv:bool) : Dom.t =
(*     let loc = !Tracing.current_loc in
    ignore(printf "if %a = %s (line %i)\n" (printExp plainCilPrinter) exp (string_of_bool tv) loc.line); *)
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
      let vnames xs = String.concat ", " (List.map (fun v -> v.var.vname) xs) in
      let mustOpen = Dom.filterValues Dom.V.opened m in
      if List.length mustOpen > 0 then
        M.report ("unclosed files: "^(vnames mustOpen));
        List.iter (fun v -> M.report ~loc:(BatList.last v.loc) "file is never closed") mustOpen;
      let mustOpenVars = List.map (fun x -> x.var) mustOpen in
      let mayOpenAll = Dom.filterValues ~may:true Dom.V.opened m in
      let mayOpen = List.filter (fun x -> not (List.mem x.var mustOpenVars)) mayOpenAll in (* ignore values that are already in mustOpen *)
      if List.length mayOpen > 0 then
        M.report ("maybe unclosed files: "^(vnames (BatList.unique ~eq:(fun a b -> a.var.vname=b.var.vname) mayOpen)));
        List.iter (fun v -> M.report ~loc:(BatList.last v.loc) "file may be never closed") mayOpen
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
      | _ -> Dom.V.create stack_var (f []) Dom.V.Close in
    Dom.add stack_var (Must v) m

  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    (* M.write ("entering function "^f.vname^(callStackStr m)); *)
    let m = if f.vname <> "main" then
      editStack (BatList.cons !Tracing.current_loc) ctx.local
    else ctx.local in [m,m]

  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    (* M.write ("leaving function "^f.vname^(callStackStr au)); *)
    let au = editStack List.tl au in
    let return_val = Dom.findOption return_var au in
    match lval, return_val with
      | Some lval, Some rval ->
          let var = lval2var lval in
          (* M.write ("setting "^var.vname^" to content of "^(Dom.V.vnames rval)); *)
          let rval = Dom.V.rebind rval var in (* change rval.var to lval *)
          Dom.add var rval (Dom.remove return_var au) (* TODO: delete tmp in return! *)
      | _ -> au

  let rec cut_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (_,o) -> `NoOffset
      | `Field (f,o) -> `Field (f, cut_offset o)

  let reachable ask e: Dom.t =
    match ask (Queries.ReachableFrom e) with
      | `LvalSet a when not (Queries.LS.is_top a) -> Dom.bot ()
           (* let to_extra (v,o) set = Dom.add (Addr.from_var_offset (v, cut_offset o)) set in *)
(*           let to_extra (v,o) set = Dom.add v set in
            Queries.LS.fold to_extra a (Dom.empty ()) *)
      (* Ignore soundness warnings, as invalidation proper will raise them. *)
      | _ -> Dom.bot ()

  let query_lv ask exp =
    match ask (Queries.MayPointTo exp) with
      | `LvalSet l when not (Queries.LS.is_top l) ->
          Queries.LS.elements l
      | _ -> []

  let rec eval_fv ask (exp:Cil.exp): varinfo option =
    match query_lv ask exp with
      | [(v,_)] -> Some v
      | _ -> None

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let m = ctx.local in
    let ret dom = [dom, Cil.integer 1, true] in
    let dummy = ret ctx.local in
    let loc = !Tracing.current_loc in
    let dloc = loc::(callStack m) in
    match f.vname with
      | "fopen" -> begin
          match lval with
            | None -> M.report "file handle is not saved!"; dummy
            | Some (lhost,offset) ->
                match lhost with
                  | Var varinfo ->
                      (* opened again, not closed before *)
                      Dom.report varinfo Dom.V.opened ("overwriting still opened file handle "^varinfo.vname) m;
                      let mustOpen, mayOpen = Dom.checkMay varinfo Dom.V.opened m in
                      if mustOpen || mayOpen then (
                        let msg = if mayOpen && not mustOpen then "file may be never closed" else "file is never closed" in
                        let xs = Dom.filterRecords varinfo Dom.V.opened m in
                        List.iter (fun x -> M.report ~loc:(BatList.last x.loc) msg) xs
                      );
                      begin match List.map (Cil.stripCasts) arglist with
                        | Const(CStr(filename))::Const(CStr(mode))::[] ->
                            ret (Dom.fopen varinfo dloc filename mode m)
                        | e::Const(CStr(mode))::[] ->
                            (* ignore(printf "CIL: %a\n" (printExp plainCilPrinter) e); *)
                            (match ctx.ask (Queries.EvalStr e) with
                              | `Str filename -> ret (Dom.fopen varinfo dloc filename mode m)
                              | _ -> M.report "no result from query"; dummy
                            )
                        | xs ->
                            M.report (String.concat ", " (List.map (fun x -> Pretty.sprint 80 (d_exp () x)) xs));
                            List.iter (fun exp -> ignore(printf "%a\n" (printExp plainCilPrinter) exp)) xs;
                            M.report "fopen needs two strings as arguments"; dummy
                      end
                  (* MayPointTo -> LValSet *)
                  | Mem exp -> M.report "TODO: save to object in memory"; dummy
          end
      | "fclose" -> begin
          match arglist with
            | [fp] -> begin match fp with
                | Lval (lhost,offset) -> begin
                    match lhost with
                      | Var varinfo ->
                          if not (Dom.mem varinfo m) then M.report ("closeing unopened file handle "^varinfo.vname);
                          Dom.report varinfo Dom.V.closed ("closeing already closed file handle "^varinfo.vname) m;
                          ret (Dom.fclose varinfo dloc m)
                      | Mem exp -> dummy
                    end
                | _ -> dummy (* TODO: only considers variables as arguments *)
              end
            | _ -> M.report "fclose needs exactly one argument"; dummy
          end
      | "fprintf" -> begin
          match arglist with
            | fp::xs -> let fp = Cil.stripCasts fp in begin match fp with
                | Lval (lhost,offset) -> begin
                    match lhost with
                      | Var varinfo ->
                          Dom.reports m varinfo [
                            false, Dom.V.closed,   "writing to closed file handle "^varinfo.vname;
                            true,  Dom.V.opened,   "writing to unopened file handle "^varinfo.vname;
                            true,  Dom.V.writable, "writing to read-only file handle "^varinfo.vname;
                          ];
                          dummy
                      | Mem exp -> dummy
                    end
                | _ -> (* List.iter (fun exp -> ignore(printf "%a\n" (printExp plainCilPrinter) exp)) arglist; *)
                       List.iter (fun exp -> M.report ("vname: "^(fst exp).vname)) (query_lv ctx.ask fp);
                       M.report "printf not Lval"; dummy
              end
            | _ -> M.report "fprintf needs at least two arguments"; dummy
          end
      | _ -> dummy

  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()
  let exitstate  () = Dom.bot ()
end

module TransparentSignatureHack: Analyses.Spec = Spec

module ThreadMCP =
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "file"
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x: lf): MCP.local_state = `File x
                let extract_l x = match x with `File x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

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

  let loc_stack = ref []
  let return_var = dummyFunDec.svar (* see base.ml: 219 *)

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
      M.report ("assigned something to file pointer "^var.vname^" (no longer safe)");
      Dom.may m var
    )else
      m
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
(*     let loc = !Tracing.current_loc in
    ignore(printf "if %a = %s (line %i)\n" (printExp plainCilPrinter) exp (string_of_bool tv) loc.line); *)
    ctx.local
  
  let body ctx (f:fundec) : Dom.t = 
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    let m = ctx.local in
    (* M.report ("return: ctx.local="^(Dom.short 50 ctx.local)); *)
    if f.svar.vname = "main" then (
      let p = (fun x -> match x.state with Dom.V.Open(_) -> true | _ -> false) in
      let vnames vars = String.concat ", " (List.map (fun v -> v.vname) vars) in
      let vars = Dom.filterVars p m in
      if List.length vars > 0 then
        M.report ("unclosed files: "^(vnames vars));
        List.iter (fun var -> M.report ~loc:var.vdecl "file is never closed") vars;
      let may_vars = Dom.filterVars ~may:true p m in
      let vars = List.filter (fun x -> not (List.mem x vars)) may_vars in
      if List.length vars > 0 then
        M.report ("maybe unclosed files: "^(vnames vars));
        List.iter (fun var -> M.report ~loc:var.vdecl "file may be never closed") vars
    );
(*     let loc = !Tracing.current_loc in
    (match exp with
      | Some exp -> ignore(printf "return %a (%i)\n" (printExp plainCilPrinter) exp loc.line)
      | _ -> ignore(1)); *)
    match exp with
      | Some(Lval(Var(varinfo),offset)) -> (* return_var := varinfo *)
          Dom.add return_var (Dom.find varinfo m) m
      | _ -> m

    
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    (* M.report ("entering function "^f.vname); *) (* TODO push loc on stack in ctx *)
    let loc = !Tracing.current_loc in
    loc_stack := loc :: !loc_stack;
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    (* M.report ("leaving function "^f.vname); *) (* TODO pop loc from stack in ctx *)
    (* let loc = !Tracing.current_loc in *)
    loc_stack := List.tl !loc_stack;
    let return_val = Dom.findOption return_var au in
    match lval, return_val with
      | Some lval, Some rval ->
          let var = lval2var lval in Dom.add var rval (Dom.remove return_var au)
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
    let dloc = Dom.V.Loc(loc :: !loc_stack) in
    match f.vname with
      | "fopen" -> begin
          match lval with
            | None -> M.report "file handle is not saved!"; dummy
            | Some (lhost,offset) ->
                match lhost with
                  | Var varinfo -> (* M.report ("file handle saved in variable "^varinfo.vname); *)
                      (* opened again, not closed before *)
                      (* if Dom.opened m varinfo then M.report ("overwriting unclosed file handle "^varinfo.vname); *)
                      Dom.report m varinfo Dom.V.opened ("overwriting still opened file handle "^varinfo.vname);
                      begin match List.map (Cil.stripCasts) arglist with
                        | Const(CStr(filename))::Const(CStr(mode))::xs -> 
                            ret (Dom.fopen m varinfo dloc filename mode)
                        | _ -> (* M.bailwith "fopen needs at two strings as arguments" *)
                                List.iter (fun exp -> ignore(printf "%a\n" (printExp plainCilPrinter) exp)) arglist;
                                M.report "fopen needs at two strings as arguments"; dummy
                      end
                  | Mem exp -> M.report "TODO: save to object in memory"; dummy
          end
      | "fclose" -> begin
          match arglist with
            | [fp] -> begin match fp with
                | Lval (lhost,offset) -> begin
                    match lhost with
                      | Var varinfo -> (* M.report ("closing file handle "^varinfo.vname); *)
                          (* if not (Dom.opened m varinfo) then M.report ("closeing unopened file handle "^varinfo.vname); *)
                          (* if      Dom.closed m varinfo  then M.report ("closeing already closed file handle "^varinfo.vname); *)
                          if not (Dom.mem varinfo m) then M.report ("closeing unopened file handle "^varinfo.vname);
                          Dom.report m varinfo Dom.V.closed ("closeing already closed file handle "^varinfo.vname);
                          ret (Dom.fclose m varinfo dloc)
                      | Mem exp -> dummy
                    end
                | _ -> dummy (* TODO: only considers variables as arguments *)
              end
            | _ -> M.bailwith "fclose needs exactly one argument"
          end
      | "fprintf" -> begin (* M.report ("fprintf: ctx.local="^(Dom.short 50 ctx.local)); *)
          match arglist with
            | fp::xs -> let fp = Cil.stripCasts fp in begin match fp with
                | Lval (lhost,offset) -> begin
                    match lhost with
                      | Var varinfo -> (* M.report ("printf to file handle "^varinfo.vname); *)
                          (* if           Dom.closed m varinfo  then M.report ("writing to closed file handle "^varinfo.vname) *)
                          (* else if not (Dom.opened m varinfo) then M.report ("writing to unopened file handle "^varinfo.vname) *)
                          (* else if not (Dom.writable m varinfo) then M.report ("writing to read-only file handle "^varinfo.vname); *)
                    
                          (* Dom.report m varinfo Dom.V.closed ("writing to closed file handle "^varinfo.vname); *)
                          (* Dom.report ~neg:true m varinfo Dom.V.opened ("writing to unopened file handle "^varinfo.vname); *)
                          (* Dom.report ~neg:true m varinfo Dom.V.writable ("writing to read-only file handle "^varinfo.vname); *)
                          Dom.reports [
                            false, m, varinfo, Dom.V.closed,   "writing to closed file handle "^varinfo.vname;
                            true,  m, varinfo, Dom.V.opened,   "writing to unopened file handle "^varinfo.vname;
                            true,  m, varinfo, Dom.V.writable, "writing to read-only file handle "^varinfo.vname;
                          ];
                          dummy
                      | Mem exp -> dummy
                    end
                | _ -> (* List.iter (fun exp -> ignore(printf "%a\n" (printExp plainCilPrinter) exp)) arglist; *)
                       List.iter (fun exp -> M.report ("vname: "^(fst exp).vname)) (query_lv ctx.ask fp);
                       M.report "printf not Lval"; dummy
              end
            | _ -> M.bailwith "fprintf needs at least two arguments"
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

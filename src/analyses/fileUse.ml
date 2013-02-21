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
      M.report ("changed file pointer "^var.vname^" (no longer safe)");
      Dom.may m var
    )else
      m
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
(*     let loc = !Tracing.current_loc in
    ignore(printf "if %a = %s (line %i)\n" (printExp plainCilPrinter) exp (string_of_bool tv) loc.line); *)
    ctx.local
  
  let body ctx (f:fundec) : Dom.t = 
    ctx.local

  let callStack () = " [call stack: "^(String.concat ", " (List.map (fun x -> string_of_int x.line) !loc_stack))^"]"

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    let m = ctx.local in
    M.write ("return: ctx.local="^(Dom.short 50 ctx.local)^(callStack ()));
    if f.svar.vname <> "main" && BatList.is_empty !loc_stack then M.write ("\n\t!!! call stack is empty for function "^f.svar.vname^" !!!");
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
    let loc = !Tracing.current_loc in
    (match exp with
      | Some exp -> ignore(printf "return %a (%i)\n" (printExp plainCilPrinter) exp loc.line)
      | _ -> ignore(1));
    match exp with
      | Some(Lval(Var(varinfo),offset)) -> (* return_var := varinfo *)
          M.write ("return variable "^varinfo.vname^" (dummy: "^return_var.vname^")");
          Dom.add return_var (Dom.find varinfo m) m
      | _ -> m

    
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    M.write ("entering function "^f.vname^(callStack ()));
    if f.vname <> "main" then (
      let loc = !Tracing.current_loc in
      loc_stack := loc :: !loc_stack  (* push loc on stack *)
    );
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    M.write ("leaving function "^f.vname^(callStack ()));
    (* let loc = !Tracing.current_loc in *)
    loc_stack := List.tl !loc_stack; (* pop loc from stack *)
    let return_val = Dom.findOption return_var au in
    match lval, return_val with
      | Some lval, Some rval -> 
          let var = lval2var lval in
          M.write ("setting "^var.vname^" to content of "^(Dom.V.vnames rval));
          let rval = Dom.V.rebind rval var in (* change rval.var to lval *)
          Dom.add var rval (Dom.remove return_var au)
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
    let dloc = loc :: !loc_stack in
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

open Messages
open Progress
open Pretty

module GU = Goblintutil

module Make 
  (Var: Analyses.VarType)  (* the equation variables *)
  (VDom: Lattice.S) (* the domain *)
  (G: Global.S) =
struct
  module Glob = G.Var
  module GDom = G.Val

  module SolverTypes = Solver.Types (Var) (VDom) (G)
  include SolverTypes

  module GCache = Cache.OneVar (G.Var)
  module WorkSet = Set.Make (Var)

  module EWC  = EffectWCon.Make(Var)(VDom)(G)
  
  let stack_d = ref 0
  let full_trace = false
  let start_c = 0  
  let max_c   : int ref = ref (-1) 
  let max_var : Var.t option ref = ref None 
  
  let is_some = function 
    | Some _ -> true
    | _ -> false
  
  let from_some = function
    | Some x -> x
    | None -> raise Not_found
  
  let histo = Hashtbl.create 1024
  let increase (v:Var.t) = 
    let set v c = 
      if not full_trace && (c > start_c && c > !max_c && (not (is_some !max_var) || not (Var.equal (from_some !max_var) v))) then begin
        if tracing then trace "sol" "Switched tracing to %a\n" Var.pretty_trace v;
        max_c := c;
        max_var := Some v
      end
    in
    try let c = Hashtbl.find histo v in
        set v (c+1);
        Hashtbl.replace histo v (c+1)
    with Not_found -> begin
        set v 1;
        Hashtbl.add histo v 1
    end
  
  let cons_unique key x xs =
    let xk = key x in
    if List.exists (fun y -> xk = key y) xs
    then xs
    else x::xs


  (* type solution'   = var_domain VMap.t * glob_domain GMap.t
		      = VDom.t Hash.Make(Var) * G.Val.t Hash.Make(G.Var)
		      = VDom.t Hash.Make(Var) * GDom.t Hash.Make(Glob)
		      = VDom.t Hash.Make(Var) * Global.S.Val.t Hash.Make(G.Var)
		      = VDom.t Hash.Make(Var) * Lattice.S.t Hash.Make(G.Var)
     start: variable * var_domain
   *)
  let compute (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' =
    let _ = print_endline "ITERATION.compute" in
    let sigma: VDom.t VMap.t = VMap.create 113 (VDom.bot ()) in
    let theta = GMap.create 113 (GDom.bot ()) in
    let vInfl = VMap.create 113 ([]: (constrain * int) list) in (* influence set for local vars (dependencies) *)
    let gInfl = GMap.create 113 ([]: (constrain * int) list) in (* influence set for global vars *)
    let todo  = VMap.create 113 ([]: (rhs * int) list) in (* rhs for one var *)
    let unsafe = ref ([]: (constrain * int) list) in (* global side effects, wird verändert *)
    let workset = ref (List.fold_right WorkSet.add initialvars WorkSet.empty) in (* workset for lhs, noch offene vars *)
    
    let rec constrainOneVar (x: variable) =
      let rhsides = 
        let notnew = VMap.mem sigma in 
          if notnew x then
            let temp = VMap.find todo x in VMap.remove todo x; temp
          else begin
            if tracing && Var.category x = 3 then trace "sol" "New %a\n" Var.pretty_trace x;
            VMap.add sigma x (VDom.bot ());  (* danger! Adding default value!!! If the datastruct refuses this,  membership test will fail -> inf. loop *)
            fst (List.fold_right (fun x (xs,i) -> (x,i)::xs, i+1) (system x) ([],0))
          end
      in 
	
      begin if rhsides=[] then ()
      else begin
        let local_state = ref (VDom.bot ()) in 
        let constrainOneRHS (f, i) =
          (if !GU.solver_progress then (incr stack_d; print_int !stack_d; flush stdout)); 
          let (nls,ngd,tc) = f (vEval ((x,f),i), GCache.cached (gEval ((x,f),i))) in
          let doOneGlobalDelta = function
            | `L (v, state) ->
              if not ( VDom.leq state (VDom.bot ()) ) then
                let oldstate = VMap.find sigma v in
                let compls = VDom.join oldstate state in
                  if not (VDom.leq compls oldstate) then begin
                    let lst = VMap.find vInfl v in
                    VMap.replace sigma v compls;
                    unsafe := lst @ !unsafe;
                    VMap.remove vInfl v
                  end
                  
            | `G (g, gstate) -> 
              if not ( GDom.leq gstate (GDom.bot ()) ) then
                let oldgstate = GMap.find theta g in
                let compgs = GDom.join oldgstate gstate in
                  if not (GDom.leq compgs oldgstate) then begin
                    let lst = GMap.find gInfl g in
                    GMap.replace theta g compgs;
                    incr Goblintutil.globals_changed;
                    if !Goblintutil.verbose then begin ignore (fprintf stderr "\n********************GLOBALS CHANGED********************* (%d)\n" !Goblintutil.globals_changed); flush stderr end;
                    unsafe := lst @ !unsafe;
                    GMap.remove gInfl g
                  end
          in
            List.iter doOneGlobalDelta ngd;
            if !GU.eclipse then show_add_work_buf (List.length tc);
            (*List.iter constrainOneVar tc;*)
            local_state := VDom.join !local_state nls;
            if !GU.solver_progress then decr stack_d 
        in
          (if !GU.solver_progress then (print_string "<"; flush stdout)); 
          List.iter constrainOneRHS rhsides;
          (if !GU.solver_progress then (print_string ">"; flush stdout));
          let old_state = VMap.find sigma x in
          (if tracing then increase x); (*update the histogram*)
          if full_trace || ((not (VDom.is_bot old_state)) && is_some !max_var && Var.equal (from_some !max_var) x) then begin
            if tracing then tracei "sol" "(%d) Entered %a.\n" !max_c Var.pretty_trace x;
            if tracing then traceu "sol" "%a\n\n" VDom.pretty_diff (!local_state, old_state)
          end;
          let new_val = VDom.join !local_state old_state in
          if not (VDom.leq new_val old_state) then begin
            VMap.replace sigma x new_val;
            let influenced_vars = ref WorkSet.empty in
            let collectInfluence ((y,f),i) = 
              VMap.replace todo y (cons_unique snd (f,i) (VMap.find todo y));             
              influenced_vars := WorkSet.add y !influenced_vars
            in
              List.iter collectInfluence (VMap.find vInfl x);
              VMap.remove vInfl x;
          if !GU.eclipse then show_add_work_buf (WorkSet.cardinal !influenced_vars);
              (*WorkSet.iter constrainOneVar !influenced_vars;*)
          end 
    end end;
    if !GU.eclipse then show_worked_buf 1
          
    and vEval (c: constrain * int) var =
      if !GU.eclipse then show_add_work_buf 1;
      (*constrainOneVar var;*)
      VMap.replace vInfl var (c :: VMap.find vInfl var);
      VMap.find sigma var 
    
    and gEval (c: constrain * int) glob = 
      GMap.replace gInfl glob (c :: GMap.find gInfl glob);
      GMap.find theta glob 

    in
      GU.may_narrow := false;
      if !GU.eclipse then show_subtask "Constant Propagation" 0;  
      List.iter (fun (v,d) -> VMap.add sigma v d) start ;

      WorkSet.iter constrainOneVar !workset;
      (*while not (WorkSet.is_empty !workset) do
        if !GU.eclipse then show_add_work_buf (WorkSet.cardinal !workset);
        WorkSet.iter constrainOneVar !workset;
        workset := WorkSet.empty;
        let recallConstraint ((y,f),i) = 
          VMap.replace todo y (cons_unique snd (f,i) (VMap.find todo y));
          workset := WorkSet.add y !workset;
        in
          List.iter recallConstraint !unsafe;
          unsafe := [];
      done;*)
      (sigma, theta)


  
  let hash_to_list hashtbl =
    VMap.fold (fun key v xs -> (key,v)::xs) hashtbl []
  

  (* system: system of constraints
     initialvars: list of identifiers?
     start: list of (type of equation variable * type of value domain)

     system -> variable list -> (variable * var_domain) list -> solution'
  *)
  let solve (system: system) (initialvars: variable list) (start:(Var.t * VDom.t) list): solution' =
    (* TODO system where every rhs is bottom *)
    let system0 = system in
    (* G or list of systems to pick from *)
    let systems = [system; system0] in
    (* 1. select an initial system gk from G *)
    let gkstart = List.hd systems in

    let rec iterate gk =
      (* 2. compute a fixed point xk of gk *)
      let xk = EWC.solve gk initialvars start in
      let sigma, theta = xk in
      let xkstart = hash_to_list sigma in

      (* 3. compute f(xk) *)
      let fxk = compute gk initialvars xkstart in (* TODO replace start with solution *)
      
      (* 4. if f(xk) == xk then return xk *)
      (* access to 'correct'? If not we need verify that returns true/false *)
      let _ = verify () gk fxk in 
      if fxk = xk then xk else
      
	(* 5. Policy improvement. Take gk+1 such that f(xk) == gk+1(xk). Goto 2 *)
	let gks = List.map (fun sys -> let fgk = compute sys initialvars xkstart in (sys,fgk)) systems in (* TODO start *)
	let gknext,fgk = List.find (fun (sys,fgk) -> fgk = fxk) gks in
	iterate gknext

    in iterate gkstart


end 

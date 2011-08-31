open Cil
open Pretty
open Analyses
module M = Messages
module H = Hashtbl
module Q = Queries

module GU = Goblintutil
module ID = ValueDomain.ID
module IntSet = SetDomain.Make (IntDomain.Integers)
module FD = ValueDomain.FD
module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module VD = ValueDomain.Compound
module LF = LibraryFunctions
module CArrays = ValueDomain.CArrays


let is_mutex_type (t: typ): bool = match t with
  | TNamed (info, attr) -> info.tname = "pthread_mutex_t" || info.tname = "spinlock_t"
  | TInt (IInt, attr) -> hasAttribute "mutex" attr
  | _ -> false

let is_fun_type (t: typ): bool = match t with
  | TFun _ -> true
  | _ -> false

let is_immediate_type t = is_mutex_type t || is_fun_type t

let is_global (a: Q.ask) (v: varinfo): bool = 
  v.vglob || match a (Q.MayEscape v) with `Bool tv -> tv | _ -> false

let is_private (a: Q.ask) (v: varinfo): bool = 
  match a (Q.IsPrivate v) with `Bool tv -> tv | _ -> false
   
module MakeSpec (Flag: ConcDomain.S) =
struct
  include Analyses.DefaultSpec

  exception Top
  module Flag = Flag

  module VD     = BaseDomain.VD
  module CPA    = BaseDomain.CPA 

  module Glob = BaseDomain.Glob 
  module Dom  = BaseDomain.Dom (Flag)

  let name = "Constant Propagation Analysis"
  let startstate () = CPA.bot (), Flag.bot ()
  let otherstate () = CPA.bot (), Flag.top ()
  let exitstate  () = CPA.bot (), Flag.get_main ()

  type cpa = CPA.t
  type flag = Flag.t
  type trans_in = Dom.t 
  type trans_out = Dom.t
  type transfer = trans_in -> trans_out
  type extra = (Cil.varinfo * Offs.t * bool) list
  type store = Dom.t
  type value = VD.t
  type address = AD.t
  type glob_fun  = Glob.Var.t -> Glob.Val.t
  type glob_diff = (Glob.Var.t * Glob.Val.t) list
  

  (**************************************************************************
   * State functions
   **************************************************************************)

  let globalize a (cpa:cpa): cpa * glob_diff  =
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) (cpa,acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
      if is_global a v && not (is_private a v) then begin
        if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
        (CPA.remove v cpa, (v,value) :: acc) 
      end else 
        (cpa,acc)
      in
        if M.tracing then M.traceu "globalize" "Done!\n";
        res
    in
      (* We fold over the local state, and collect the globals *)
      CPA.fold add_var cpa (cpa, [])

  let sync ctx: Dom.t * glob_diff = 
    let cpa,fl = ctx.local in
    let cpa, diff = if Flag.is_multi fl then globalize ctx.ask cpa else (cpa,[]) in
      (cpa,fl), diff

  (** [get st addr] returns the value corresponding to [addr] in [st] 
   *  adding proper dependencies *)
  let rec get a (gs: glob_fun) (st,fl: store) (addrs:address): value =
    let firstvar = if M.tracing then try (List.hd (AD.to_var_may addrs)).vname with _ -> "" else "" in
    if M.tracing then M.traceli "get" ~var:firstvar "Address: %a\nState: %a\n" AD.pretty addrs CPA.pretty st;
    (* Finding a single varinfo*offset pair *)
    let res = 
    let f_addr (x, offs) = 
      (* get hold of the variable value, either from local or global state *)
      let var = if (!GU.earlyglobs || Flag.is_multi fl) && is_global a x then
        match CPA.find x st with
          | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; gs x)
          | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)
      else begin
        if M.tracing then M.tracec "get" "Singlethreaded mode.\n";
        CPA.find x st 
      end
      in
      VD.eval_offset (get a gs (st,fl)) var offs    
    in 
    let f x =
      match Addr.to_var_offset x with
      | [x] -> f_addr x                    (* norml reference *)
      | _ when Addr.is_null x -> VD.bot () (* null pointer *)
      | _ -> `Int (ID.top ())              (* string pointer *)
    in
    (* We form the collecting function by joining *)
    let f x a = VD.join (f x) a in
      (* Finally we join over all the addresses in the set. If any of the
       * addresses is a topped value, joining will fail. *)
      try AD.fold f addrs (VD.bot ()) with SetDomain.Unsupported _ -> VD.top ()
    in
      if M.tracing then M.traceu "get" "Result: %a\n" VD.pretty res;
      res

  (** [set st addr val] returns a state where [addr] is set to [val] *)
  let set a ?(effect=true) (gs:glob_fun) (st,fl: store) (lval: AD.t) (value: value): store =
    let firstvar = if M.tracing then try (List.hd (AD.to_var_may lval)).vname with _ -> "" else "" in
    if M.tracing then M.tracel "set" ~var:firstvar "lval: %a\nvalue: %a\nstate: %a\n" AD.pretty lval VD.pretty value CPA.pretty st;
    (* Updating a single varinfo*offset pair. NB! This function's type does
     * not include the flag. *)
    let update_one_addr (x, offs) nst: cpa = 
      (* Check if we need to side-effect this one. We no longer generate
       * side-effects here, but the code still distinguishes these cases. *)
      if (!GU.earlyglobs || Flag.is_multi fl) && is_global a x then 
        (* Check if we should avoid producing a side-effect, such as updates to
         * the state when following conditional guards. *)
        if not effect then nst
        else begin
         (* Here, an effect should be generated, but we add it to the local
          * state, waiting for the sync function to publish it. *)
         CPA.add x (VD.update_offset (CPA.find x nst) offs value) nst
        end 
      else
       (* Normal update of the local state *)
       CPA.add x (VD.update_offset (CPA.find x nst) offs value) nst
    in 
    let update_one x (y: cpa) =
      match Addr.to_var_offset x with
        | [x] -> update_one_addr x y
        | _ -> y
    in try 
      (* We start from the current state and an empty list of global deltas,
       * and we assign to all the the different possible places: *)
      let nst = AD.fold update_one lval st in
      (* If the address was definite, then we just return it. If the address
       * was ambiguous, we have to join it with the initial state. *)
      let nst = if AD.cardinal lval > 1 then CPA.join st nst else nst in
        (nst,fl)
    with 
      (* If any of the addresses are unknown, we ignore it!?! *)
      | SetDomain.Unsupported _ -> M.warn "Assignment to unknown address"; (st,fl)

  let set_many a (gs:glob_fun) (st,fl as store: store) lval_value_list: store =
    (* Maybe this can be done with a simple fold *)
    let f (acc: store) ((lval:AD.t),(value:value)): store = 
      set a gs acc lval value 
    in
      (* And fold over the list starting from the store turned wstore: *)
      List.fold_left f store lval_value_list

  (* The function for invalidating a list of addresses *)
  let set_top (st,fl:store) (lvals: AD.t list) = ()

  let join_writes (st1,gl1) (st2,gl2) = 
    (* It's the join of the local state and concatenate the global deltas, I'm
     * not sure in which order! *)
    (Dom.join st1 st2, gl1 @ gl2)

  let rem_many (st,fl: store) (v_list: varinfo list): store = 
    let f acc v = CPA.remove v acc in
      List.fold_left f st v_list, fl

  let es_to_string f (es,fl) = 
    let short_fun x = 
      match x.vtype, CPA.find x es with
        | TPtr (t, attr), `Address a 
            when (not (AD.is_top a)) 
              && List.length (AD.to_var_may a) = 1 
              && not (is_immediate_type t) 
              -> 
            let cv = List.hd (AD.to_var_may a) in 
              "ref " ^ VD.short 26 (CPA.find cv es)
        | _, v -> VD.short 30 v
    in
    let args_short = List.map short_fun f.sformals in
      Printable.get_short_list (GU.demangle f.svar.vname ^ "(") ")" 80 args_short

  (**************************************************************************
   * Initializing my variables
   **************************************************************************)

  let return_varstore = ref dummyFunDec.svar
  let return_varinfo () = !return_varstore 
  let return_var () = AD.from_var (return_varinfo ())
  let return_lval (): lval = (Var (return_varinfo ()), NoOffset)

  let heap_var loc = AD.from_var (BaseDomain.get_heap_var loc)

  let init () = 
    return_varstore := makeVarinfo false "RETURN" voidType;
    H.clear BaseDomain.heap_hash

  (**************************************************************************
   * Abstract evaluation functions
   **************************************************************************)

  (* Evaluate Cil.binop for two abstract values: *)
  let evalbinop (op: binop) (a1:value) (a2:value): value = 
    (* We define a conversion function for the easy cases when we can just use
     * the integer domain operations. *)
    let int_op =
      match op with 
        | PlusA -> ID.add
        | MinusA -> ID.sub
        | Mult -> ID.mul
        | Div -> ID.div
        | Mod -> ID.rem
        | Lt -> ID.lt
        | Gt -> ID.gt
        | Le -> ID.le
        | Ge -> ID.ge
        | Eq -> ID.eq
        | Ne -> ID.ne
        | BAnd -> ID.bitand
        | BOr -> ID.bitor
        | BXor -> ID.bitxor
        | Shiftlt -> ID.shift_left
        | Shiftrt -> ID.shift_right
        | LAnd -> ID.logand
        | LOr -> ID.logor
        | _ -> (fun x y -> ID.top ())
    in let float_op =
      match op with 
        | PlusA  -> print_endline "base:evalbinop:op=plus"; FD.add
        | MinusA -> print_endline "base:evalbinop:op=minus"; FD.sub
        | Mult   -> print_endline "base:evalbinop:op=mult"; FD.mul
        | Div    -> print_endline "base:evalbinop:op=div"; FD.div
        | Mod    -> print_endline "base:evalbinop:op=mod"; FD.rem
        | Lt -> print_endline "base:evalbinop:op=lt"; FD.lt
        | Gt -> print_endline "base:evalbinop:op=gt"; FD.gt
        | Le -> print_endline "base:evalbinop:op=le"; FD.le
        | Ge -> print_endline "base:evalbinop:op=ge"; FD.ge
        | Eq -> print_endline "base:evalbinop:op=eq"; FD.eq
        | Ne -> print_endline "base:evalbinop:op=ne"; FD.ne
        | BAnd -> FD.bitand
        | BOr -> FD.bitor
        | BXor -> FD.bitxor
        | Shiftlt -> FD.shift_left
        | Shiftrt -> FD.shift_right
        | LAnd -> FD.logand
        | LOr -> FD.logor
        | _ -> print_endline "base:evalbinop:top"; (fun x y -> FD.top ())
    (* An auxiliary function for ptr arithmetic on array values. *)
    in let addToAddr n (addr:Addr.t) =
      match Addr.to_var_offset addr with
        | [x,`Index (i, offs)] when ID.is_int i -> 
            Addr.from_var_offset (x, `Index (ID.add i n, offs))
        | [x,`NoOffset] ->
            Addr.from_var_offset (x, `Index (n, `NoOffset))          
        | [_] -> raise Top 
        | _ -> addr
    in
      (* The main function! *)
      match a1,a2 with
        (* For the integer values, we apply the domain operator *)
        | `Int v1, `Int v2 -> print_endline "base:evalbinop:int*int"; `Int (int_op v1 v2)
	(* Floats *)
        | `Float v1, `Float v2 -> print_endline "base:evalbinop:float*float"; `Float (float_op v1 v2)
        | `Int v1, `Float v2 -> (match ID.to_int v1 with
		| Some v1 -> `Float (float_op (FD.of_float (Int64.to_float v1)) v2)
		| None -> raise Top)
        | `Float v1, `Int v2 -> (match ID.to_int v2 with
		| Some v2 -> `Float (float_op v1 (FD.of_float (Int64.to_float v2)))
		| None -> raise Top)
        (* For address +/- value, we try to do some elementary ptr arithmetic *)
        | `Address p, `Int n  -> begin
            try match op with
              (* For array indexing, e[i] we have *)
              | IndexPI -> `Address (AD.map (addToAddr n) p)
              (* Pointer addition e + i, it's the same: *)
              | PlusPI -> `Address (AD.map (addToAddr n) p)
              (* Pointer subtracted by a value (e-i) is very similar *)
              | MinusPI -> let n = ID.neg n in
                  `Address (AD.map (addToAddr n) p)
              | Mod -> `Int (ID.top ()) (* we assume that address is actually casted to int first*)
              | _ -> `Address (AD.top ())
            with
              | Top -> `Address (AD.top ())
          end
        (* If both are pointer values, we can subtract them and well, we don't
         * bother to find the result, but it's an integer. *)
        | `Address p1, `Address p2 -> begin
            let single a = try AD.cardinal a = 1 with _ -> false in 
            let eq x y = 
              let xl = AD.to_var_must x in
              let yl = AD.to_var_must y in
              if List.length xl = 1 && List.length yl = 1 
              then ID.of_bool (List.exists2 (fun x y -> x.vid = y.vid) xl yl)
              else ID.top ()
            in  
            match op with
              | MinusPP -> `Int (ID.top ())
              | Eq -> `Int (if (single p1)&&(single p2) then (eq p1 p2) else ID.top())
              | Ne -> `Int (if (single p1)&&(single p2) then ID.lognot (eq p1 p2) else ID.top())
              | _ -> VD.top ()
          end
        (* For other values, we just give up! *)
        | `Bot, _ -> `Bot
        | _, `Bot -> `Bot
        | _ -> VD.top ()


  (* Evaluating Cil's unary operators. Yes, this is easy! *)
  let evalunop op a1 =
    let int_op =
      match op with
        | Cil.Neg  -> ID.neg
        | Cil.BNot -> ID.bitnot
        | Cil.LNot -> ID.lognot
    in
    let float_op =
      match op with
        | Cil.Neg  -> FD.neg
        | Cil.BNot -> FD.bitnot
        | Cil.LNot -> FD.lognot
    in
      match a1 with
        | `Int v1 -> `Int (int_op v1)
        | `Float v1 -> `Float (float_op v1)
        | `Bot -> `Bot
        | _ -> VD.top ()

  (* Auxiliary function to append an additional offset to a given offset. *)
  let rec add_offset ofs add = 
    match ofs with
      | `NoOffset -> add
      | `Field (fld, `NoOffset) -> `Field (fld, add)
      | `Field (fld, ofs) -> `Field (fld, add_offset ofs add)
      | `Index (exp, `NoOffset) -> `Index (exp, add)
      | `Index (exp, ofs) -> `Index (exp, add_offset ofs add)

  (* We need the previous function with the varinfo carried along, so we can
   * map it on the address sets. *)
  let add_offset_varinfo add ad = 
    match Addr.to_var_offset ad with 
      | [x,ofs] -> Addr.from_var_offset (x, add_offset ofs add)
      | _ -> ad
      
  (* evaluate value using our "query functions" *)
  let eval_rv_pre (ask: Q.ask) exp pr =
    let binop op e1 e2 =
      let equality () = 
        match ask (Q.ExpEq (e1,e2)) with
          | `Int 0L -> Some false
          | `Int _ -> Some true
          | _ -> None
      in
      match op with
        | Cil.MinusA
        | Cil.MinusPI 
        | Cil.MinusPP when equality () = Some true -> Some (`Int (ID.of_int 0L))
        | Cil.MinusA
        | Cil.MinusPI
        | Cil.MinusPP when equality () = Some false -> Some (`Int (ID.of_excl_list [0L]))
        | Cil.Le
        | Cil.Ge when equality () = Some true -> Some (`Int (ID.of_bool true))
        | Cil.Lt
        | Cil.Gt when equality () = Some true -> Some (`Int (ID.of_bool false))
        | Cil.Eq -> (match equality () with Some tv -> Some (`Int (ID.of_bool tv)) | None -> None)
        | Cil.Ne -> (match equality () with Some tv -> Some (`Int (ID.of_bool (not tv))) | None -> None)
        | _ -> None
    in
    match exp with
      | Cil.BinOp (op,arg1,arg2,_) -> binop op arg1 arg2
      | _ -> None

  (* The evaluation function as mutually recursive eval_lv & eval_rv *)
  let rec eval_rv (a: Q.ask) (gs:glob_fun) (st: store) (exp:exp): value = 
    let rec do_offs def = function 
      | Field (fd, offs) -> begin
          match Goblintutil.is_blessed (TComp (fd.fcomp, [])) with
            | Some v -> do_offs (`Address (AD.singleton (Addr.from_var_offset (v,convert_offset a gs st (Field (fd, offs)))))) offs
                | None -> do_offs def offs  
          end
      | Index (_, offs) -> do_offs def offs
      | NoOffset -> def
    in
    (* we have a special expression that should evalueate to top ... *)
    if exp = MyCFG.unknown_exp then VD.top () else
    (* First we try with query functions --- these are currently more precise.
     * Ideally we would meet both values, but we fear types might not match. (bottom) *)
    match eval_rv_pre a exp st with
      | Some x -> x 
      | None -> 
    (* query functions were no help ... now try with values*)
    match Cil.constFold true exp with
      (* Integer literals *)
      | Cil.Const (Cil.CInt64 (num,typ,str)) -> `Int (ID.of_int num)
      (* Float literals *)
      | Cil.Const (Cil.CReal (num,typ,str)) -> let _ = printf "base:eval_rv:Found Const Float: %f\n" num in `Float (FD.of_float num)
      (* String literals *)
      | Cil.Const (Cil.CStr _)
      | Cil.Const (Cil.CWStr _) -> `Address (AD.str_ptr ())
      (* Variables and address expressions *)
      | Cil.Lval (Var v, ofs) -> do_offs (get a gs st (eval_lv a gs st (Var v, ofs))) ofs
      | Cil.Lval (Mem e, ofs) -> do_offs (get a gs st (eval_lv a gs st (Mem e, ofs))) ofs
      (* Binary operators *)
      | Cil.BinOp (op,arg1,arg2,typ) -> 
          let a1 = eval_rv a gs st arg1 in
          let a2 = eval_rv a gs st arg2 in
            evalbinop op a1 a2
      (* Unary operators *)
      | Cil.UnOp (op,arg1,typ) -> 
          let a1 = eval_rv a gs st arg1 in
            evalunop op a1
      (* The &-operator: we create the address abstract element *)
      | Cil.AddrOf lval -> `Address (eval_lv a gs st lval)
      (* CIL's very nice implicit conversion of an array name [a] to a pointer
       * to its first element [&a[0]]. *)
      | Cil.StartOf lval -> 
          let array_ofs = `Index (ID.of_int 0L, `NoOffset) in
          let array_start ad = 
            match Addr.to_var_offset ad with
              | [x, offs] -> Addr.from_var_offset (x, add_offset offs array_ofs) 
              | _ -> ad
          in
          `Address (AD.map array_start (eval_lv a gs st lval))
       | Cil.CastE  (t, Const (CStr _)) -> VD.top ()
      (* Most casts are currently just ignored, that's probably not a good idea! *)
       | Cil.CastE  (t, exp) -> begin
           match t,eval_rv a gs st exp with
             | Cil.TPtr (_,_), `Top -> `Address (AD.top ())
              | Cil.TPtr _, `Int a when Some Int64.zero = ID.to_int a -> 
                  `Address (AD.null_ptr ())
              | Cil.TInt _, `Address a when AD.equal a (AD.null_ptr()) -> 
                  `Int (ID.of_int Int64.zero)
             | _, s -> s
       end
      | _ -> print_endline "base:eval_rv:VD.top"; VD.top ()
  (* A hackish evaluation of expressions that should immediately yield an
   * address, e.g. when calling functions. *)
  and eval_fv a (gs:glob_fun) st (exp:exp): AD.t = 
    match exp with
      | Cil.Lval lval -> eval_lv a gs st lval
      | _ -> 
         match (eval_rv a gs st exp) with
           | `Address x -> x
           | _          -> M.bailwith "Problems evaluating expression to function calls!"
  (* A function to convert the offset to our abstract representation of
   * offsets, i.e.  evaluate the index expression to the integer domain. *)
  and convert_offset a (gs:glob_fun) (st: store) (ofs: Cil.offset) = 
    match ofs with 
      | Cil.NoOffset -> `NoOffset
      | Cil.Field (fld, ofs) -> `Field (fld, convert_offset a gs st ofs)
      | Cil.Index (exp, ofs) -> 
          let exp_rv = eval_rv a gs st exp in
          match exp_rv with
            | `Int i -> `Index (i, convert_offset a gs st ofs)
            | `Top   -> `Index (ID.top (), convert_offset a gs st ofs) 
            | _ -> M.bailwith "Index not an integer value"
  (* Evaluation of lvalues to our abstract address domain. *)
  and eval_lv (a: Q.ask) (gs:glob_fun) st (lval:lval): AD.t = 
    let rec do_offs def = function 
      | Field (fd, offs) -> begin
          match Goblintutil.is_blessed (TComp (fd.fcomp, [])) with
            | Some v -> do_offs (AD.singleton (Addr.from_var_offset (v,convert_offset a gs st (Field (fd, offs))))) offs
                | None -> do_offs def offs  
          end
      | Index (_, offs) -> do_offs def offs
      | NoOffset -> def
    in
    match lval with 
      | Cil.Var x, NoOffset when (not x.vglob) && Goblintutil.is_blessed x.vtype<> None ->  
         begin match Goblintutil.is_blessed x.vtype with
           | Some v -> AD.singleton (Addr.from_var v)
           | _ ->  AD.singleton (Addr.from_var_offset (x, convert_offset a gs st NoOffset))
         end
      (* The simpler case with an explicit variable, e.g. for [x.field] we just
       * create the address { (x,field) } *)
      | Cil.Var x, ofs -> 
         if x.vglob 
         then AD.singleton (Addr.from_var_offset (x, convert_offset a gs st ofs))
         else do_offs (AD.singleton (Addr.from_var_offset (x, convert_offset a gs st ofs))) ofs
      (* The more complicated case when [exp = & x.field] and we are asked to
       * evaluate [(\*exp).subfield]. We first evaluate [exp] to { (x,field) }
       * and then add the subfield to it: { (x,field.subfield) }. *)
      | Cil.Mem n, ofs -> begin
          match (eval_rv a gs st n) with 
            | `Address adr -> do_offs (AD.map (add_offset_varinfo (convert_offset a gs st ofs)) adr) ofs
            | _ ->  let str = Pretty.sprint ~width:80 (Pretty.dprintf "%a " d_lval lval) in
                M.debug ("Failed evaluating "^str^" to lvalue"); do_offs (AD.top ()) ofs
          end 



  (**************************************************************************
   * Auxilliary functions
   **************************************************************************)

  let rec bot_value a (gs:glob_fun) (st: store) (t: Cil.typ): value = 
    let rec bot_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let bot_field nstruct fd = ValueDomain.Structs.replace nstruct fd (bot_value a gs st fd.Cil.ftype) in
        List.fold_left bot_field nstruct compinfo.Cil.cfields 
    in
      match t with
        | Cil.TInt _ -> `Bot (*`Int (ID.bot ()) -- should be lower than any int or address*)
        | Cil.TPtr _ -> `Address (AD.bot ())
        | Cil.TComp ({Cil.cstruct=true} as ci,_) -> `Struct (bot_comp ci)
        | Cil.TComp ({Cil.cstruct=false},_) -> `Union (ValueDomain.Unions.bot ())
           | Cil.TArray (_, None, _) -> `Array (ValueDomain.CArrays.bot ())
        | Cil.TArray (ai, Some exp, _) -> begin
            let default = `Array (ValueDomain.CArrays.bot ()) in
            match eval_rv a gs st exp with
              | `Int n -> begin
                  match ID.to_int n with
                    | Some n -> `Array (ValueDomain.CArrays.make (Int64.to_int n) (bot_value a gs st ai))
                    | _ -> default
                end
              | _ -> default
          end
        | Cil.TNamed ({Cil.ttype=t}, _) -> bot_value a gs st t
        | _ -> `Bot 

  let rec init_value a (gs:glob_fun) (st: store) (t: Cil.typ): value = 
    let rec init_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let init_field nstruct fd = ValueDomain.Structs.replace nstruct fd (init_value a gs st fd.Cil.ftype) in
        List.fold_left init_field nstruct compinfo.Cil.cfields 
    in
      match t with
        | t when is_mutex_type t -> `Top
        | Cil.TInt _ -> `Int (ID.top ())
        | Cil.TFloat _ -> `Float (FD.top ())
        | Cil.TPtr _ -> `Address (AD.top ())
        | Cil.TComp ({Cil.cstruct=true} as ci,_) -> `Struct (init_comp ci)
        | Cil.TComp ({Cil.cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
        | Cil.TArray _ -> bot_value a gs st t
        | Cil.TNamed ({Cil.ttype=t}, _) -> init_value a gs st t
        | _ -> `Top 

  let rec top_value (st: store) (t: Cil.typ): value = 
    let rec top_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let top_field nstruct fd = ValueDomain.Structs.replace nstruct fd (top_value st fd.Cil.ftype) in
        List.fold_left top_field nstruct compinfo.Cil.cfields 
    in
      match t with
        | Cil.TInt _ -> `Int (ID.top ())
        | Cil.TFloat _ -> `Float (FD.top ())
        | Cil.TPtr _ -> `Address (AD.top ())
        | Cil.TComp ({Cil.cstruct=true} as ci,_) -> `Struct (top_comp ci)
        | Cil.TComp ({Cil.cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
        | Cil.TArray _ -> `Array (ValueDomain.CArrays.top ())
        | Cil.TNamed ({Cil.ttype=t}, _) -> top_value st t
        | _ -> `Top 

  let invariant a (gs:glob_fun) st exp tv = 
    (* We use a recursive helper function so that x != 0 is false can be handled
     * as x == 0 is true etc *)
    let rec helper (op: Cil.binop) (lval: Cil.lval) (value: value) (tv: bool) = 
      match (op, lval, value, tv) with
        (* The true-branch where x == value: *)
        | Cil.Eq, x, value, true -> 
            if M.tracing then M.trace "invariant" "Yes, success! %a equals %a\n\n" Cil.d_lval x VD.pretty value;
            Some (x, value)
        (* The false-branch for x == value: *)
        | Cil.Eq, x, value, false -> begin
            match value with
              | `Int n -> begin
                  match ID.to_int n with
                    | Some n ->
                        (* When x != n, we can return a singleton exclusion set *)
                        if M.tracing then M.trace "invariant" "Yes, success! %a is not %Ld\n\n" Cil.d_lval x n;
                        Some (x, `Int (ID.of_excl_list [n]))
                    | None -> None
                end
              | `Address n ->
                  if M.tracing then M.trace "invariant" "Yes, success! %a is not NULL\n\n" Cil.d_lval x;
                  let x_rv = 
                    match eval_rv a gs st (Cil.Lval x) with
                     | `Address a -> a
                     | _ -> AD.top() in
                  Some (x, `Address (AD.diff x_rv n))                  
              | _ -> 
                (* We can't say anything else, exclusion sets are finite, so not
                 * being in one means an infinite number of values *)
                if M.tracing then M.trace "invariant" "Failed! (not a definite value)\n\n";
                None
          end
        | Cil.Ne, x, value, _ -> helper Cil.Eq x value (not tv)
        | Cil.Lt, x, value, _ -> begin
           let range_from x = if tv then ID.ending (Int64.sub x 1L) else ID.starting x in
           let limit_from = if tv then ID.maximal else ID.minimal in
           match value with
             | `Int n -> begin 
                 match limit_from n with
                   | Some n ->
                        if M.tracing then M.trace "invariant" "Yes, success! %a is not %Ld\n\n" Cil.d_lval x n;
                        Some (x, `Int (range_from n))
                   | None -> None
             end
             | _ -> None
           end
        | Cil.Le, x, value, _ -> begin
           let range_from x = if tv then ID.ending x else ID.starting (Int64.add x 1L) in
           let limit_from = if tv then ID.maximal else ID.minimal in
           match value with
             | `Int n -> begin 
                 match limit_from n with
                   | Some n ->
                        if M.tracing then M.trace "invariant" "Yes, success! %a is not %Ld\n\n" Cil.d_lval x n;
                        Some (x, `Int (range_from n))
                   | None -> None
             end
             | _ -> None
           end
        | Cil.Gt, x, value, _ -> helper Cil.Le x value (not tv)
        | Cil.Ge, x, value, _ -> helper Cil.Lt x value (not tv)
        | _ -> 
            if M.tracing then M.trace "invariant" "Failed! (operation not supported)\n\n";
            None
    in
      if M.tracing then M.tracel "invariant" "expression: %a is %B\n" Cil.d_exp exp tv;
        let null_val typ =
          match typ with
            | Cil.TPtr _ -> `Address (AD.null_ptr())
            | _      -> `Int (ID.of_int 0L) 
        in
        let rec derived_invariant exp tv = 
        match exp with 
          (* Since we only handle equalities the order is not important *)
          | Cil.BinOp(op, Cil.Lval x, rval, typ) -> helper op x (eval_rv a gs st rval) tv
          | Cil.BinOp(op, rval, Cil.Lval x, typ) -> helper op x (eval_rv a gs st rval) tv
          | Cil.BinOp(op, Cil.CastE (xt,x), Cil.CastE (yt,y), typ) when xt = yt 
            -> derived_invariant (Cil.BinOp (op, x, y, typ)) tv
          (* Cases like if (x) are treated like if (x != 0) *)
          | Cil.Lval x -> 
            (* There are two correct ways of doing it: "if ((int)x != 0)" or "if (x != (typeof(x))0))"
             * Because we try to avoid casts (and use a more precise address domain) we use the latter *)
              helper Cil.Ne x (null_val (Cil.typeOf exp)) tv
          | Cil.UnOp (Cil.LNot,uexp,typ) -> derived_invariant uexp (not tv)
          | _ -> 
              if M.tracing then M.trace "invariant" "Failed! (expression %a not understood)\n\n" Cil.d_exp exp;
              None
      in
      let is_some_bot x =
        match x with
          | `Int n ->  ID.is_bot n
          | `Float n ->  FD.is_bot n
          | `Address n ->  AD.is_bot n
          | `Struct n ->  ValueDomain.Structs.is_bot n
          | `Union n ->  ValueDomain.Unions.is_bot n
          | `Array n ->  ValueDomain.CArrays.is_bot n
          | `Blob n ->  ValueDomain.Blobs.is_bot n
          | `List n ->  ValueDomain.Lists.is_bot n
          | `Bot -> false (* HACK: bot is here due to typing conflict (we do not cast approprietly) *)
          | `Top -> false
      in
        match derived_invariant exp tv with
          | Some (lval, value) -> 
              let addr = eval_lv a gs st lval in
               if (AD.is_top addr) then
                         st
                       else
                         let oldval = get a gs st addr in
                         let new_val = VD.meet oldval value in
                (* make that address meet the invariant, i.e exclusion sets
                 * will be joined *)
                           if is_some_bot new_val 
                           then raise Analyses.Deadcode
                else if VD.is_bot new_val 
                then set a gs st addr value ~effect:false
                else set a gs st addr new_val ~effect:false
          | None -> st

  let set_savetop ask (gs:glob_fun) st adr v =
    match v with
      | `Top -> set ask gs st adr (top_value st (AD.get_type adr))
      | v -> set ask gs st adr v


 (**************************************************************************
  * Simple defs for the transfer functions 
  **************************************************************************)
  
  let assign ctx (lval:lval) (rval:exp)  = (**print_endline "base:assign";**)
    let is_list_init () =
      match lval, rval with
      | (Var a, Field (fi,NoOffset)), AddrOf((Var b, NoOffset)) 
          when !GU.global_initialization && a.vid = b.vid 
            && fi.fcomp.cname = "list_head" 
            && (fi.fname = "prev" || fi.fname = "next")
          -> Some a
      | _ -> None
    in
    match is_list_init () with
      | Some a when !GU.use_list_type -> 
          begin 
            set ctx.ask ctx.global ctx.local 
                (AD.singleton (Addr.from_var a)) 
                (`List (ValueDomain.Lists.bot ()))
          end
      | _ -> 
    let rval_val = eval_rv ctx.ask ctx.global ctx.local rval in
    let lval_val = eval_lv ctx.ask ctx.global ctx.local lval in
    let not_local xs = 
      let not_local x = 
        match Addr.to_var_may x with
          | [x] -> is_global ctx.ask x 
          | _ -> Addr.is_top x
      in
      AD.is_top xs || AD.exists not_local xs
    in
    begin match rval_val, lval_val with
      | `Address adrs, lval
        when (not !GU.global_initialization) && !GU.kernel && not_local lval && not (AD.is_top adrs) ->
          let find_fps e xs = Addr.to_var_must e @ xs in
          let vars = AD.fold find_fps adrs [] in
          let funs = List.filter (fun x -> isFunctionType x.vtype) vars in
          List.iter (fun x -> ctx.spawn x (otherstate ())) funs  
      | _ -> ()
    end;
    set_savetop ctx.ask ctx.global ctx.local lval_val rval_val

  let branch ctx (exp:exp) (tv:bool) : store =
    (* First we want to see, if we can determine a dead branch: *)
    match eval_rv ctx.ask ctx.global ctx.local exp with
      (* For a boolean value: *)
      | `Int value when (ID.is_bool value) -> 
          (* to suppress pattern matching warnings: *)
          let fromJust x = match x with Some x -> x | None -> assert false in
          let v = fromJust (ID.to_bool value) in
            (* Eliminate the dead branch and just propagate to the true branch *)
            if v == tv then ctx.local else raise Deadcode
      | `Bot -> raise Deadcode
      (* Otherwise we try to impose an invariant: *)
      | _ -> invariant ctx.ask ctx.global ctx.local exp tv 

  let body ctx f = 
    (* First we create a variable-initvalue pair for each varaiable *)
    let init_var v = (AD.from_var v, init_value ctx.ask ctx.global ctx.local v.vtype) in
    (* Apply it to all the locals and then assign them all *)
    let inits = List.map init_var f.slocals in
      set_many ctx.ask ctx.global ctx.local inits

  let return ctx exp fundec =
    if fundec.svar.vname = "__goblint_dummy_init" then begin
      let (cp,fl) = ctx.local in
      if Flag.is_multi fl then ctx.local else (cp, Flag.get_main ())
    end else
      let nst = rem_many ctx.local (fundec.sformals @ fundec.slocals) in
        match exp with
          | None -> nst
          | Some exp -> set ctx.ask ctx.global nst (return_var ()) (eval_rv ctx.ask ctx.global ctx.local exp)


  (**************************************************************************
   * Auxiliary functions for function calls
   **************************************************************************)

  (* The normal haskell zip that throws no exception *)
  let rec zip x y = match x,y with
    | (x::xs), (y::ys) -> (x,y) :: zip xs ys
    | _ -> []

  (* From a list of values, presumably arguments to a function, simply extract
   * the pointer arguments. *)
  let get_ptrs (vals: value list): address list = 
    let f x acc = match x with
      | `Address adrs when AD.is_top adrs -> 
          M.warn "Unkown address given as function argument"; acc
      | `Address adrs when AD.to_var_may adrs = [] -> acc
      | `Address adrs -> 
          let typ = AD.get_type adrs in
            if is_fun_type typ then acc else adrs :: acc
      | `Top -> M.warn "Unkown value type given as function argument"; acc
      | _ -> acc
    in 
      List.fold_right f vals []

  (* Hmm... top level?  Watch out ... *)
  let empty = AD.empty ()

  (* Get the list of addresses accessable immediately from a given address, thus
   * all pointers within a structure should be considered, but we don't follow
   * pointers. We return a flattend representation, thus simply an address (set). *)
  let reachable_from_address (ask: Q.ask) (gs:glob_fun) st (adr: address): address =
    if M.tracing then M.tracei "reachability" "Checking for %a\n" AD.pretty adr;
    let rec reachable_from_value (value: value) =
      if M.tracing then M.trace "reachability" "Checking value %a\n" VD.pretty value;
      match value with
        | `Top -> 
            let typ = AD.get_type adr in
            let warning = "Unknown value in " ^ AD.short 40 adr ^ " could be an escaped pointer address!" in
              if is_immediate_type typ then () else M.warn warning; empty 
        | `Bot -> (*M.debug "A bottom value when computing reachable addresses!";*) empty
        | `Address adrs when AD.is_top adrs -> 
            let warning = "Unknown address in " ^ AD.short 40 adr ^ " has escaped." in
              M.warn warning; empty
        (* The main thing is to track where pointers go: *)
        | `Address adrs -> adrs
        (* Unions are easy, I just ingore the type info. *)
        | `Union (t,e) -> reachable_from_value e
        (* For arrays, we ask to read from an unknown index, this will cause it
         * join all its values. *)
        | `Array a -> reachable_from_value (ValueDomain.CArrays.get a (ID.top ()))
        | `Blob e -> reachable_from_value e
        | `List e -> reachable_from_value (`Address (ValueDomain.Lists.entry_rand e))
        | `Struct s -> ValueDomain.Structs.fold (fun k v acc -> AD. join (reachable_from_value v) acc) s empty
        | `Int _ -> empty
        | `Float _ -> empty
    in
    let res = reachable_from_value (get ask gs st adr) in
      if M.tracing then M.traceu "reachability" "Reachable addresses: %a\n" AD.pretty res;
      res

  (* The code for getting the variables reachable from the list of parameters.
   * This section is very confusing, because I use the same construct, a set of
   * addresses, as both AD elements abstracting individual (ambiguous) addresses
   * and the workset of visited addresses. *)
  let reachable_vars (ask: Q.ask) (args: address list) (gs:glob_fun) (st: store): address list =
    if M.tracing then M.traceli "reachability" "Checking reachable arguments from [%a]!\n" (d_list ", " AD.pretty) args;
    (* We begin looking at the parameters: *)
    let argset = List.fold_right AD.join args empty in
    let workset = ref argset in
    (* And we keep a set of already visited variables *)
    let visited = ref empty in
      while not (AD.is_empty !workset) do
        visited := AD.union !visited !workset;
        (* ok, let's visit all the variables in the workset and collect the new variables *)
        let visit_and_collect (var: AD.elt) (acc: address): address =
          let var = AD.singleton var in (* Very bad hack! Pathetic really! *)
            AD.union (reachable_from_address ask gs st var) acc in
        let collected = AD.fold visit_and_collect !workset empty in
          (* And here we remove the already visited variables *)
          workset := AD.diff collected !visited 
      done;
      (* Return the list of elements that have been visited. *)
      if M.tracing then M.traceu "reachability" "All reachable vars: %a\n" AD.pretty !visited;
      List.map AD.singleton (AD.elements !visited)

  let invalidate ask (gs:glob_fun) (st:store) (exps: Cil.exp list): store = 
    (* To invalidate a single address, we create a pair with its corresponding
     * top value. *)
    let invalidate_address st a = 
      let t = AD.get_type a in
          (a, top_value st t)
    in
    (* We define the function that evaluates all the values that an address
     * expression e may point to *)
    let invalidate_exp e = 
      match eval_rv ask gs st e with
         (*a null pointer is invalid by nature*)
        | `Address a when AD.equal a (AD.null_ptr()) -> []
        | `Address a when not (AD.is_top a) -> 
            List.map (invalidate_address st) (reachable_vars ask [a] gs st)
        | `Int _ -> []
        | _ -> let expr = sprint ~width:80 (Cil.d_exp () e) in
            M.warn ("Failed to invalidate unknown address: " ^ expr); []
    in
    (* We concatMap the previous function on the list of expressions. *)
    let invalids = List.concat (List.map invalidate_exp exps) in
      set_many ask gs st invalids

  (* Variation of the above for yet another purpose, uhm, code reuse? *)
  let collect_funargs ask (gs:glob_fun) (st:store) (exps: exp list) = 
    let do_exp e = 
      match eval_rv ask gs st e with
        | `Address a when AD.equal a (AD.null_ptr ()) -> []
        | `Address a when not (AD.is_top a) -> 
            let rble = reachable_vars ask [a] gs st in
              if M.tracing then
                M.trace "collect_funargs" "%a = %a\n" AD.pretty a (d_list ", " AD.pretty) rble;
              rble
        | _-> []
    in
      List.concat (List.map do_exp exps)
       
  (* interpreter end *)
  
  let get_fl (_,fl) = fl
  
  let hash    (x,y,_)             = Hashtbl.hash (x,y)
  let equal   (x1,x2,_) (y1,y2,_) = CPA.equal x1 y1 && Flag.equal x2 y2
  let leq     (x1,x2,_) (y1,y2,_) = CPA.leq   x1 y1 && Flag.leq   x2 y2 
  let compare (x1,x2,_) (y1,y2,_) = 
    match CPA.compare x1 y1 with 
      | 0 -> Flag.compare x2 y2
      | x -> x
      
  let convertToQueryLval x =
    let rec offsNormal o = 
      let toInt i = 
        match ValueDomain.ID.to_int i with
          | Some x -> Const (CInt64 (x,IInt, None))
          | _ -> mkCast (Const (CStr "unknown")) Cil.intType
                                       
      in
      match o with
        | `NoOffset -> `NoOffset
        | `Field (f,o) -> `Field (f,offsNormal o) 
        | `Index (i,o) -> `Index (toInt i,offsNormal o) 
    in
    match x with
      | ValueDomain.AD.Addr.Addr (v,o) ->[v,offsNormal o]
      | _ -> []
  
  let addrToLvalSet a = 
    let add x y = Q.LS.add y x in
    try
      AD.fold (fun e c -> List.fold_left add c (convertToQueryLval e)) a (Q.LS.empty ())
    with SetDomain.Unsupported _ -> Q.LS.top ()
        
  let eval_funvar ctx fval: varinfo list =
    try 
      AD.to_var_may (eval_fv ctx.ask ctx.global ctx.local fval)
    with SetDomain.Unsupported _ -> 
      M.warn ("Unknown call to function " ^ Pretty.sprint 100 (d_exp () fval) ^ ".");
      [dummyFunDec.svar]

  let query ctx (q:Q.t) = 
    match q with
      | Q.EvalFunvar e ->
        begin
          let fs = eval_funvar ctx e in
(*          Messages.report ("Base: I should know it! "^string_of_int (List.length fs));*)
          `LvalSet (List.fold_left (fun xs v -> Q.LS.add (v,`NoOffset) xs) (Q.LS.empty ()) fs)
        end
      | Q.EvalInt e -> begin
            match eval_rv ctx.ask ctx.global ctx.local e with
              | `Int e -> (match ID.to_int e with Some i -> `Int i | _ -> `Top) 
              | _ -> print_endline (sprint 80 (d_exp () e)); `Top
          end
      | Q.MayPointTo e -> begin
          match eval_rv ctx.ask ctx.global ctx.local e with 
            | `Address a -> `LvalSet (addrToLvalSet a)
            | _ -> `Top
          end
      | Q.ReachableFrom e -> begin
          match eval_rv ctx.ask ctx.global ctx.local e with
            | `Top -> `Top
            | `Bot -> `Bot
            | `Address a when AD.is_top a -> 
                `LvalSet (Q.LS.top ())   
            | `Address a ->
                let xs = List.map addrToLvalSet (reachable_vars ctx.ask [a] ctx.global ctx.local) in 
                let addrs = List.fold_left Q.LS.join (Q.LS.empty ()) xs in
                `LvalSet addrs
            | _ -> `LvalSet (Q.LS.empty ())      
          end
      | Q.SingleThreaded -> `Int (Q.ID.of_bool (not (Flag.is_multi (get_fl ctx.local))))
      | Q.CurrentThreadId when (Flag.is_bad (get_fl ctx.local)) -> `Top
      | Q.CurrentThreadId -> `Int 1L
      | _ -> Q.Result.top ()

  (**************************************************************************
   * Function calls
   **************************************************************************)

  let rec collect_spawned ctx args: (varinfo * Dom.t) list = 
    let flist = collect_funargs ctx.ask ctx.global ctx.local args in
    let f addr = 
      let var = List.hd (AD.to_var_may addr) in
      let g = Cilfacade.getdec var in 
      let args = List.map (fun x -> MyCFG.unknown_exp) g.sformals in
      let ents = enter_func_wo_spawns (Analyses.swap_st ctx (otherstate ())) None var args in
      List.map (fun (_,s) -> var, s) ents
    in 
    let g a acc = try 
      let r = f a in r @ acc 
    with 
      | Not_found 
      | Failure "hd" -> acc
      | x -> M.debug ("Ignoring exception: " ^ Printexc.to_string x); acc 
    in
      List.fold_right g flist [] 

  and forkfun ctx (lv: lval option) (f: varinfo) (args: exp list) : (varinfo * Dom.t) list = 
    let cpa,fl = ctx.local in
    match LF.classify f.vname args with 
      (* handling thread creations *)
      | `ThreadCreate (start,ptc_arg) -> begin        
          let start_addr = eval_fv ctx.ask ctx.global ctx.local start in
          let start_vari = List.hd (AD.to_var_may start_addr) in
          try
            (* try to get function declaration *)
            let _ = Cilfacade.getdec start_vari in 
            let sts = enter_func_wo_spawns (swap_st ctx (cpa, Flag.get_multi ())) None start_vari [ptc_arg]  in
            List.map (fun (_,st) -> start_vari, st) sts
          with Not_found -> 
            M.warn ("creating an thread from unknown function " ^ start_vari.vname);
            [start_vari,(cpa, Flag.get_multi ())]
        end
      | `Unknown _ -> begin
          if M.tracing then M.traceli "forkfun" ~var:f.vname ~subsys:["reachability"] "Tracing reachable functions for %s\n" f.vname;
          let args = 
            match LF.get_invalidate_action f.vname with
              | Some fnc -> fnc `Write  args
              | None -> args
          in
          let res = collect_spawned ctx args in
            if M.tracing then M.traceu "forkfun" "Done!\n"; res
        end
      | _ ->  []

  and enter_func ctx lval fn args : (Dom.t * Dom.t) list = 
    let forks = forkfun ctx lval fn args in
    let spawn (x,y) = ctx.spawn x y in List.iter spawn forks ;
    enter_func_wo_spawns ctx lval fn args

  and enter_func_wo_spawns ctx lval fn args : (Dom.t * Dom.t) list = 
    let cpa,fl as st = ctx.local in
    let make_entry pa context =
      (* If we need the globals, add them *)
      let new_cpa = if not (!GU.earlyglobs || Flag.is_multi fl) then CPA.filter_class 2 cpa else CPA.bot () in 
      (* Assign parameters to arguments *)
      let new_cpa = CPA.add_list pa new_cpa in
      let new_cpa = CPA.add_list_fun context (fun v -> CPA.find v cpa) new_cpa in
        st, (new_cpa, fl) 
    in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv ctx.ask ctx.global st) args in
    (* List of reachable variables *)
    let reachable = List.concat (List.map AD.to_var_may (reachable_vars ctx.ask (get_ptrs vals) ctx.global st)) in
    (* generate the entry states *)
    let fundec = Cilfacade.getdec fn in
      [make_entry (zip fundec.sformals vals) reachable]


  let special_fn ctx (lv:lval option) (f: varinfo) (args: exp list) = 
(*    let heap_var = heap_var !GU.current_loc in*)
    let forks = forkfun ctx lv f args in
    let spawn (x,y) = ctx.spawn x y in List.iter spawn forks ;
    let cpa,fl as st = ctx.local in
    let gs = ctx.global in
    let map_true x = x, (Cil.integer 1), true in
    match LF.classify f.vname args with 
      | `Unknown "list_add" when !GU.use_list_type -> 
          begin match args with
            | [ AddrOf (Var elm,next);(AddrOf (Var lst,NoOffset))] -> 
                begin
                  let ladr = AD.singleton (Addr.from_var lst) in
                  match get ctx.ask ctx.global ctx.local ladr with
                    | `List ld ->
                      let eadr = AD.singleton (Addr.from_var elm) in
                      let eitemadr = AD.singleton (Addr.from_var_offset (elm, convert_offset ctx.ask ctx.global ctx.local next)) in
                      let new_list = `List (ValueDomain.Lists.add eadr ld) in
                      let s1 = set ctx.ask ctx.global ctx.local ladr new_list in
                      let s2 = set ctx.ask ctx.global s1 eitemadr (`Address (AD.singleton (Addr.from_var lst))) in
                      [map_true s2]
                    | _ -> [map_true (set ctx.ask ctx.global ctx.local ladr `Top)]
                end
            | _ -> M.bailwith "List function arguments are strange/complicated."
          end
      | `Unknown "list_del" when !GU.use_list_type -> 
          begin match args with
            | [ AddrOf (Var elm,next) ] -> 
                begin
                  let eadr = AD.singleton (Addr.from_var elm) in
                  let lptr = AD.singleton (Addr.from_var_offset (elm, convert_offset ctx.ask ctx.global ctx.local next)) in
                  let lprt_val = get ctx.ask ctx.global ctx.local lptr in
                  let lst_poison = `Address (AD.singleton (Addr.from_var ListDomain.list_poison)) in
                  let s1 = set ctx.ask ctx.global ctx.local lptr (VD.join lprt_val lst_poison) in
                  match get ctx.ask ctx.global ctx.local lptr with
                    | `Address ladr ->
                      begin match get ctx.ask ctx.global ctx.local ladr with
                        | `List ld ->
                          let del_ls = ValueDomain.Lists.del eadr ld in
                          let s2 = set ctx.ask ctx.global s1 ladr (`List del_ls) in
                          [map_true s2]
                        | _ -> [map_true s1]
                      end
                    | _ -> [map_true s1]
                end
            | _ -> M.bailwith "List function arguments are strange/complicated."
          end
      | `Unknown "exit" ->  raise Deadcode
      | `Unknown "abort" -> raise Deadcode
      | `Unknown "spinlock_check" -> 
          begin match lv with
            | Some x -> map_true (assign ctx x (List.hd args)) :: []
            | None -> map_true ctx.local :: []
          end
      (* handling thread creations *)
      | `ThreadCreate (f,x) -> 
          GU.multi_threaded := true;
          let new_fl = Flag.join fl (Flag.get_main ()) in
            [map_true (cpa, new_fl)]
      (* handling thread joins... sort of *)
      | `ThreadJoin (id,ret_var) -> 
          begin match (eval_rv ctx.ask gs st ret_var) with
            | `Int n when n = ID.of_int 0L -> [map_true (cpa,fl)]
            | _      -> [map_true (invalidate ctx.ask gs st [ret_var])] 
          end
      | `Malloc  -> begin
        match lv with
          | Some lv -> 
            let heap_var = 
              if !GU.malloc_may_fail 
              then AD.join (heap_var !GU.current_loc) (AD.null_ptr ()) 
              else heap_var !GU.current_loc
            in 
            [map_true (set_many ctx.ask gs st [(heap_var, `Blob (VD.bot ()));  
                                       (eval_lv ctx.ask gs st lv, `Address heap_var)])]
          | _ -> [map_true st]
        end
      | `Calloc -> 
        begin match lv with
          | Some lv -> 
              let heap_var = BaseDomain.get_heap_var !GU.current_loc in
                [map_true (set_many ctx.ask gs st [(AD.from_var heap_var, `Array (CArrays.make 0 (`Blob (VD.bot ())))); 
                                           (eval_lv ctx.ask gs st lv, `Address (AD.from_var_offset (heap_var, `Index (ID.of_int 0L, `NoOffset))))])]
          | _ -> [map_true st]
        end
      (* Handling the assertions *)
      | `Unknown "__assert_rtn" -> raise Deadcode (* gcc's built-in assert *) 
      | `Assert e -> begin
          (* evaluate the assertion and check if we can refute it *)
          let expr () = sprint ~width:80 (d_exp () e) in
          match eval_rv ctx.ask gs st e with 
            (* If the assertion is known to be false/true *)
            | `Int v when ID.is_bool v -> 
                (* Warn if it was false; ignore if true! The None case
                  * should not happen! *)
                (match ID.to_bool v with
                  | Some false -> M.warn_each ("Assertion \"" ^ expr () ^ "\" will fail")
                  | _ -> ()); 
                (* Just propagate the state *)
                [map_true st]
            | `Bot -> [map_true st]
            | _ -> begin 
                if !GU.debug then begin
                  M.warn_each ("Assertion \"" ^ expr () ^ "\" is unknown");
                  [map_true st]
                end else
                  (* make the state meet the assertion in the rest of the code *)
                  [map_true (invariant ctx.ask gs st e true)]
              end
        end
      | _ -> begin
          let lv_list = 
            match lv with
              | Some x -> [Cil.mkAddrOrStartOf x]
              | None -> []
          in
          match LF.get_invalidate_action f.vname with
            | Some fnc -> [map_true (invalidate ctx.ask gs st (lv_list @ (fnc `Write  args)))];
            | None -> (
                M.warn ("Function definition missing for " ^ f.vname);
                let st_expr (v:varinfo) (value) a = 
                  if is_global ctx.ask v then Cil.mkAddrOf (Var v, NoOffset) :: a else a
                in
                (* This here is just to see of something got spawned. *)
                let flist = collect_funargs ctx.ask gs st args in
                let f addr = 
                  let var = List.hd (AD.to_var_may addr) in
                  let _ = Cilfacade.getdec var in true
                in 
                let g a acc = try let r = f a in r || acc with _ -> acc in
                let (cpa,fl as st) = invalidate ctx.ask gs st (CPA.fold st_expr cpa (lv_list @ args)) in
                  if List.fold_right g flist false then begin
                    (* Copy-pasted from the thread-spawning code above: *)
                    GU.multi_threaded := true;
                    let new_fl = Flag.join fl (Flag.get_main ()) in
                      [map_true (cpa,new_fl)]
                  end else [map_true st]
              )
        end

  let leave_func ctx (lval: lval option) fexp (f: varinfo) (args: exp list) (after: Dom.t) : Dom.t =
    let combine_one (loc,lf as st: Dom.t) ((fun_st,fun_fl) as fun_d: Dom.t) = 
      (* This function does miscelaneous things, but the main task was to give the
       * handle to the global state to the state return from the function, but now
       * the function tries to add all the context variables back to the callee.
       * Note that, the function return above has to remove all the local
       * variables of the called function from cpa_s. *)
      let add_globals (cpa_s,fl_s) (cpa_d,fl_dl) = 
        (* Remove the return value as this is dealt with separately. *)
        let cpa_s = CPA.remove (return_varinfo ()) cpa_s in
        let new_cpa = CPA.fold CPA.add cpa_s cpa_d in
          (new_cpa, fl_s)
      in 
      let return_var = return_var () in
      let return_val = 
        if CPA.mem (return_varinfo ()) fun_st
        then get ctx.ask ctx.global fun_d return_var 
        else VD.top ()  
      in
      let st = add_globals (fun_st,fun_fl) st in
        match lval with
          | None      -> st
          | Some lval -> set_savetop ctx.ask ctx.global st (eval_lv ctx.ask ctx.global st lval) return_val
     in
     combine_one ctx.local after

end

module Spec = MakeSpec (ConcDomain.Trivial)
module Main = MakeSpec (ConcDomain.Simple)

module BaseMCP = 
  MCP.ConvertToMCPPart
        (Main)
        (struct let name = "base" 
                let depends = []
                type lf = Main.Dom.t
                let inject_l x = `Base x
                let extract_l x = match x with `Base x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Main.Glob.Val.t
                let inject_g x = `Base x
                let extract_g x = match x with `Base x -> x | _ -> raise MCP.SpecificationConversionError
         end)

(** Abstract Domains for floats. These are domains that support the C
  * operations on float values. *)

module type S =
sig
  include Lattice.S
  type fkind = [`Float | `Double | `LongDouble]

  (** {b Accessing values of the ADT} *)

  val to_float: t -> float option
  (** Return a single float value if the value is a known constant, otherwise
    * don't return anything. *)
  val of_float: float * fkind -> t
  (** Transform an float literal to your internal domain representation. *)
  val is_float: t -> bool
  (** Checks if the element is a definite float value. If this function
    * returns [true], the above [to_int] should return a real value. *)

  val to_bool: t -> bool option
  (** Give a boolean interpretation of an abstract value if possible, otherwise
    * don't return anything.*)
  val of_bool: bool * fkind -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_float 1.0] and [of_bool false = of_float 0.0]. *)
  val is_bool: t -> bool
  (** Checks if the element is a definite boolean value. If this function
    * returns [true], the above [to_bool] should return a real value. *)
  val to_excl_list: t -> float list option
  (* Gives a list representation of the excluded values if possible. *)
  val of_excl_list: float list -> t
  (* Creates a exclusion set from a given list of floats. *)
  val is_excl_list: t -> bool
  (* Checks if the element is an exclusion set. *)
(*  val of_interval: float -> float -> t*)
  val starting   : float -> t
  val ending     : float -> t
  val maximal    : t -> float option
  val minimal    : t -> float option
  
  (** {b Arithmetic operators} *)

  val neg: t -> t
  (** Negating an float value: [-x] *)
  val add: t -> t -> t
  (** Addition: [x + y] *)
  val sub: t -> t -> t
  (** Subtraction: [x - y] *)
  val mul: t -> t -> t
  (** Multiplication: [x * y] *)
  val div: t -> t -> t
  (** Division: [x / y] *)
  val rem: t -> t -> t
  (** float remainder: [x % y] *) 
                       
  (** {b Comparison operators} *)

  val lt: t -> t -> t
  (** Less than: [x < y] *)
  val gt: t -> t -> t
  (** Greater than: [x > y] *)
  val le: t -> t -> t
  (** Less than or equal: [x <= y] *)
  val ge: t -> t -> t
  (** Greater than or equal: [x >= y] *)
  val eq: t -> t -> t
  (** Equal to: [x == y] *)
  val ne: t -> t -> t
  (** Not equal to: [x != y] *)

  (** {b Bit operators} *)

  val bitnot: t -> t
  (** Bitwise not (one's complement): [~x] *)
  val bitand: t -> t -> t
  (** Bitwise and: [x & y] *)
  val bitor : t -> t -> t
  (** Bitwise or: [x | y] *)
  val bitxor: t -> t -> t
  (** Bitwise exclusive or: [x ^ y] *)
  val shift_left : t -> t -> t
  (** Shifting bits left: [x << y] *)
  val shift_right: t -> t -> t
  (** Shifting bits right: [x >> y] *)

  (** {b Logical operators} *)

  val lognot: t -> t
  (** Logical not: [!x] *)
  val logand: t -> t -> t
  (** Logical and: [x && y] *)
  val logor : t -> t -> t
  (** Logical or: [x || y] *)
  val logxor : t -> t -> t
  (** Logical xor: [x xor y] *)
end
(** The signature of integral value domains. They need to support all float
  * operations that are allowed in C *)


exception Unknown
(** An exception that can be raised when the result of a computation is unknown.
  * This is caught by lifted domains and will be replaced by top. *)

exception Error
(** An exception that can be raised when an arithmetic error occurs. This is
  * caught by lifted domains and the evaluation will then be set to bot, which
  * signifies an error in computation *)

(** {b Predefined domains} *)

module Floats : S with type t = float * [`Float | `Double | `LongDouble]
(** The floats with their natural orderings. Calling [top] and [bot] will
  * raise exceptions. *)

module type SigConversion =
sig
  type t
  val mode: int ref
  val current_mode: unit -> string
  val doubleToFloat: float -> float
  val doubleToFloatDomain: t -> t
  val bitstringOfInt64: int64 -> string
  val bitstringOfFloat: float -> string
  val of_int: int64 option -> t
end
module Conversion (Base: S) : SigConversion with type t = Base.t
(** Conversion between different kinds of floats *)

module FlatPureFloats : S with type t = float * [`Float | `Double | `LongDouble]
(** The floats with flattened orderings. Calling [top] and [bot] or [join]ing
    or [meet]ing inequal elements will raise exceptions. *)

module Flattened : S with type t = [`Top | `Lifted of float * [`Float | `Double | `LongDouble] | `Bot]
(** This is the typical flattened float domain used in Kildall's constant
  * propagation. *)

(*module Lifted : S with type t = [`Top | `Lifted of float | `Bot]*)
(** Artificially bounded floats in their natural ordering. *)

module Trier 
: S with type t = [
    | `Excluded of SetDomain.Make(Floats).t
    | `Definite of Floats.t
    | `Bot
    ]
(** The Trier domain. The Flattened float domain is topped by exclusion sets.
  * Good for analysing branches. *)


(** {b Domain constructors} *)

module Flat (Base: S): S
(** Creates a flat value domain, where all ordering is lost. Arithmetic
  * operations are lifted such that only lifted values can be evaluated
  * otherwise the top/bot is simply propagated with bot taking precedence over
  * top. *)

module Lift (Base: S): S
(** Just like {!Value.Flat} except the order is preserved. *)

module Interval : S
(** Interval domain with float-s --- use with caution! *)

module IncExcInterval : S with type t = [ | `Excluded of Interval.t| `Included of Interval.t ] 
(** Inclusive and exclusive intervals. Warning: NOT A LATTICE *)

module ManyFloats : S 
module FloatDomList : S 

(** {b Boolean domains} *)

module type BooleansNames = 
sig
  val truename: string (** The name of the [true] abstract value *)
  val falsename: string (** The name of the [false] abstract value *)
end
(** Parameter signature for the [MakeBooleans] functor. *)

module MakeBooleans (Names: BooleansNames): S with type t = bool
(** Creates an abstract domain for floats represented by boolean values. *)

module Booleans: S with type t = bool
(** Boolean abstract domain, where true is output "True" and false is output
  * "False" *)

module None: S with type t = unit
(** Domain with nothing in it. *)

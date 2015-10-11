(* ENVIRONMENTS *)

(* The environment module implementation. *)
module Env = struct
  (* We represent an environment as a list of bindings, with each binding
     represented as a pair. *)
  type 'a env = (string * 'a) list;;

  exception NotBound

  let to_list (e : 'a env) : (string * 'a) list = e   

  (* Create an empty environment. *)
  let empty_env():'a env = []

  (* A new binding is prepended to an environment.  Any existing binding
     with the same name is shadowed, assuming lookup proceeds from front to
     back.  *)
  let add_binding (s:string) (item:'a) (e:'a env) :'a env = (s,item)::e

  (* Combine two environments, with bindings from the second one shadowing
     bindings from the first one. *)
  let combine_envs (e1:'a env) (e2: 'a env):'a env = e2@e1 

  (* Lookup a name in the environment, throwing NotBound if the name is not
     bound in the environment. *)
  let lookup (s:string) (e: 'a env) :'a =
    try List.assoc s e with
	Not_found -> raise NotBound
end;;


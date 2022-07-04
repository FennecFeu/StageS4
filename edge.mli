module type OrderedPrintableType =
  sig
    type t
    val compare : t -> t -> int
    val print_t : t -> unit
  end

module type Edge =
  sig
    (* The type related to the nodes *)
    type node

    (* The module related to the node type *)
    module NodeType : OrderedPrintableType with type t = node
    
    (* The type related to the edge *)
    type t

    (*
      @requires None
      @ensures Returns an edge with the given two nodes at the extremities of the edge
     *)
    val edge : node -> node -> t

    (*
      @requires None
      @ensures Returns a result of the comparison between two edges.
     *)
    val compare : t -> t -> int

    (*
      @requires None
      @ensures Returns the predecessor node of the edge
     *)
    val get_pred : t -> node

    (*
      @requires None
      @ensures Returns the successor node of the edge
     *)
    val get_succ : t -> node

    (*
      @requires None
      @ensures Returns the predecessor and the successor nodes of the edge
     *)
    val get_nodes : t -> node * node
      
  end


(* Functor to create edges which type t relate to the given ordered type *)
module MakeEdge(N:OrderedPrintableType) : Edge with type node = N.t

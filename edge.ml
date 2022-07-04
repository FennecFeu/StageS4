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

module MakeEdge(N:OrderedPrintableType) =
  struct
    type data = N.t option
    type node = N.t

    module DataType = N
    module NodeType = N
              
    type t = node * data * node

    let edge a b = (a, None, b)

    let compare (a, b, c) (d, e, f) =
      if NodeType.compare a d = 0 then
        if NodeType.compare c f = 0
        then match b, e with
               | None, None -> 0
               | None, Some v -> -1
               | Some v, None -> 1
               | Some u, Some v -> DataType.compare u v
        else NodeType.compare c f
      else NodeType.compare a d

    let get_pred (a, _, _) = a

    let get_succ (_, _, a) = a

    let get_nodes (a, _, b) = (a, b)
  end



module type Graph =
  sig

    (* The type related to the nodes of the graph *)
    type node

    (* The type related to the edges of the graph *)
    type edge

    (* The module related to the ordered type of the nodes of the graph *)
    module NodeType : Set.OrderedType with type t = node

    (* The module related to the ordered type of the edges of the graph *)
    module EdgeType : Set.OrderedType with type t = edge

    (* The module related to the set which contains elements with node type *)
    module NodeSet : Set.S with type elt = node

    (* The module related to the set which contains elements with edge type *)
    module EdgeSet : Set.S with type elt = edge

    (* The type related to the graph *)
    type graph

    (*
      @requires None
      @ensures Returns an empty graph
     *)
    val empty : graph

    (*
      @requires None
      @ensures Returns true if the graph is empty. If not, returns false.
     *)
    val is_empty : graph -> bool

    (*
      @requires None
      @ensures Returns a set of nodes which are all the successors of the given node.
      @raises Not_found if the node does not belong to the graph
     *)
    val succs : graph -> node -> NodeSet.t

    (*
      @requires None
      @ensures Returns a set of nodes which are all the predecessors of the given
 node.
      @raises Not_found if the node does not belong to the graph
     *) 
    val preds : graph -> node -> NodeSet.t

    (*
      @requires None
      @ensures Returns the graph with the new node added
     *)
    val add_node : node -> graph -> graph

    (*
      @requires non
      @ensures Returns the graph with the new edge added. If one of the two nodes of the edge does not belong to the graph, we add that node too.
     *)
    val add_edge : edge -> graph -> graph

    (*
      @requires None
      @ensures Returns the node with the minimum value in the graph.
      @raises Not_found if there is no node that can be return
     *)
    val get_min_node : graph -> node

    (*
      @requires None
      @ensures Returns the first edge going from the first node to the second one
      @raises Not_found if there is no edge that can be return
     *)
    val get_edge : graph -> node -> node -> edge

    (*
      @requires None
      @ensures Given the operation and a graph, returns from an entry a modified value after calling the operation on all the nodes of the graph.
     *)
    val fold_node : ('a -> node -> 'a) -> 'a -> graph -> 'a

    val print_node : node -> unit
      
  end
  
(* Functor to create a graph which type edge relate to the given ordened type *)
module MakeGraph(E:Edge.Edge) : Graph with type node = E.NodeType.t and type edge = E.t
                                                          

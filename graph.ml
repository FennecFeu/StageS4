module type Graph =
  sig

    (* The type related to the nodes of the graph *)
    type node

    (* The type related to the edges of the graph *)
    type edge

    (* The module related to the ordered type of the graph *)
    module NodeType : Set.OrderedType with type t = node

    (* The module related to the set which contains elements with node type *)
    module NodeSet : Set.S with type elt = node

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
    val add_edge : node -> node -> graph -> graph

    (*
      @requires None
      @ensures Returns the node with the minimum value in the graph.
      @raises Not_found if there is no node that can be return
     *)
    val get_min_node : graph -> NodeSet.t -> node

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
      
  end


(* Functor to create a graph with an Ordered Type *)
module MakeGraph1(N:Set.OrderedType)(E:Set.OrderedType) =
  struct

    type node = N.t

    type edge = E.t

    module NodeType = N

    module EdgeType = E

    module NodeSet = Set.Make(NodeType)

    module EdgeSet = Set.Make(EdgeType)

    module NodeMap = Map.Make(NodeType)

    module DoubleNode =
      struct
        type t = node * node
        let compare (a, b) (c, d) =
          let t1 = NodeType.compare a c in
          if t1 = 0 then NodeType.compare b d else t1
      end

    module EdgeMap = Map.Make(DoubleNode)
                   
    type graph =
      { succs : NodeSet.t NodeMap.t;
        preds : NodeSet.t NodeMap.t;
        edges : EdgeSet.t EdgeMap.t
      }

    let empty =
      { succs = NodeMap.empty;
        preds = NodeMap.empty;
        edges = EdgeMap.empty
      }

    let is_empty g = g = empty

    let succs g n = NodeMap.find n g.succs

    let preds g n = NodeMap.find n g.preds

    let add_node n g =
      if NodeMap.mem n g.succs then g
      else
        { succs = NodeMap.add n NodeSet.empty g.succs;
          preds = NodeMap.add n NodeSet.empty g.preds;
          edges = g.edges
        }

    let add_edge u v g =
      let g2 = add_node u (add_node v g) in
    
  end

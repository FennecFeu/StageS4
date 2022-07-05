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

    (*
      @requires The node is in the graph
      @ensures Returns the incidence number of the node in the graph.
     *)
    val incidence_number : graph -> node -> int

    (*
      @requires None
      @ensures Print the given node in the console
     *)
    val print_node : node -> unit
      
  end


(* Functor to create a graph with an Ordered Type *)
module MakeGraph(E:Edge.Edge) =
  struct

    type node = E.NodeType.t

    type edge = E.t

    module NodeType = E.NodeType

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
        edges : edge EdgeMap.t
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

    let add_edge e g =
      let u, v = E.get_nodes e in
      let g2 = add_node u (add_node v g) in
      { succs = NodeMap.add u (NodeSet.add v (NodeMap.find u g2.succs)) g2.succs;
        preds = NodeMap.add v (NodeSet.add u (NodeMap.find v g2.preds)) g2.preds;
        edges = EdgeMap.add (u, v) e g2.edges;
      }

    let get_min_node g =
      let (k, _) = NodeMap.min_binding g.succs in k

    let get_edge g u v =
      EdgeMap.find (u, v) g.edges

    let incidence_number g v =
      (NodeSet.cardinal (NodeMap.find v g.preds)) + (NodeSet.cardinal (NodeMap.find v g.succs))

    let fold_node f acc g =
      let f2 k _ accm = f accm k in
      NodeMap.fold f2 g.succs acc

    let print_node = E.NodeType.print_t
    
  end

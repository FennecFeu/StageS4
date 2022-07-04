module type FindMatchedPattern =
  sig

    module GType : Graph.Graph

    module Matching : Set.OrderedType

    module ResultSet : Set.S with type elt = Matching.t

    val find_matched_pattern : GType.graph -> GType.graph -> ResultSet.t

    val print_result : Matching.t -> unit
  end


(* Functor to create the function to find subgraph isomorphisms *)
module MakeFindMatchedPattern(G:Graph.Graph) : FindMatchedPattern with module GType = G

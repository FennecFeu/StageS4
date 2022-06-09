open Graph

module Int =
  struct
    type t = int
    let compare a b = a-b
  end

module GraphInt = MakeGraph(Int)
                

module FindMatchedPattern(G:Graph.Graph) =
  struct

    module NodeMap = Map.Make(G.NodeType)
    
    type matching = G.NodeType.t NodeMap.t

    module ResultSet = Set.S with type elt = MatchSet.t

    type state =
      { matching_peers : M.matching;
        matched_g : G.NodeSet.t;
        succs_g : G.NodeSet.t;
        preds_g : G.NodeSet.t
      }
    
    (*
      @requires None
      @ensures Returns a set of all matchings between q and a subgraph of g where q and the subgraph have the same structure
     *)
    let find_matched_pattern q g =

      let is_compatible m n =
        failwith"TODO"
      in

      let fsyn s matched_q succs_q preds_q m n =
        failwith "TODO"
      in
        

      let rec partial_match m (ls, matched_q, succs_q, preds_q) =
        if ls = [] then (ResultSet.empty, matched_q, succs_q, preds_q)
        else let tmp_succs_q = G.NodeSet.remove m succs_q in
             let acc1 =
               List.fold_left
                 (fun acc state -> failwith "TODO"
                          
      
      if G.is_empty q then ResultSet.empty
      else let m = G.get_min_node q in
           (* m is the minimum according to its ordered type and m does not belong to matched_q *)
           let all_starts =
             G.fold_node (fun acc n -> if is_compatible m n then ResultSet.add (NodeMap.add m n NodeMap.empty) acc)
               ResultSet.empty g in
           let ls =
             ResultSet.fold
               (fun mp acc -> let n = NodeMap.find m mp in
                              let new_state =
                                { matching_peers = mp;
                                  matched_g = G.NodeSet.add n G.NodeSet.empty;
                                  succs_g = G.succs g n;
                                  preds_g = G.preds g n;
                                } in
                              new_state::acc)
               all_starts [] in
           let matched_q = G.NodeSet.add m G.NodeSet.empty in
           let succs_q = G.succs q m in
           let preds_q = G.preds q m in
           let (all_states, _, _, _) = NodeSet.fold partial_match (G.NodeSet.union succs_q preds_q) (ls, matched_q, succs_q, preds_q) in
           List.fold_left (fun acc state -> ResultSet.add state.matching_peers acc) ResultSet.empty all_states
                                              
  end

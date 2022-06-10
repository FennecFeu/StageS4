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
        true (* Temporairement *)
      in

      let fsyn s matched_q succs_q preds_q m n =
        G.NodeSet.for_all 
      in

      let create_new_state m n tmp_s =
        let new_matching_peers = NodeMap.add m n tmp_s.matching_peers in
        let new_matched_g = G.NodeSet.add n tmp_s.matched_g in
        let new_succs_g = G.NodeSet.union tmp_s.succs_g (G.NodeSet.diff (G.succs g n) new_matched_g) in
        let new_preds_g = G.NodeSet.union tmp_s.preds_g (G.NodeSet.diff (G.preds g n) new_matched_g) in
        { matching_peers = new_matching_peers;
          matched_g = new_matched_g;
          succs_g = new_succs_g;
          preds_g = new_preds_g;
        }
        
        

      let rec partial_match m (ls, matched_q, succs_q, preds_q) =
        if ls = [] then (ResultSet.empty, matched_q, succs_q, preds_q)
        else let tmp_succs_q = G.NodeSet.remove m succs_q in
             let acc1 =
               List.fold_left
                 (fun acc s ->
                   G.NodeSet.fold
                     (fun n aux_acc ->
                       if not (is_compatible m n) then acc
                       else let tmp_s = {s with succs_g = G.NodeSet.remove n s.succs_g;} in
                            if not fsyn tmp_s matched_q tmp_succs_q preds_q m n then aux_acc
                            else let new_s = create_new_state m n tmp_s in
                                 new_s::aux_acc)
                     s.succs_g acc)
                 [] ls in
             let acc2 =
               List.fold_left
                 (fun acc s ->
                   G.NodeSet.fold
                     (fun n aux_acc ->
                       if not (is_compatible m n) then acc
                       else let tmp_s = {s with preds_g = G.NodeSet.remove n s.preds_g;} in
                            if not fsyn tmp_s matched_q succs_q tmp_preds_q m n then aux_acc
                            else let new_s = create_new_state m n tmp_s in
                                 new_s::aux_acc)
                     s.succs_g acc)
                 [] ls in
             let new_matched_q = G.NodeSet.add m matched_q in
             let new_succs_q = G.NodeSet.union succs_q (G.NodeSet.diff (G.succs q m) new_matched_q) in
             let new_preds_q = G.NodeSet.union preds_q (G.NodeSet.diff (G.preds q m) new_matched_q) in
             G.NodeSet.fold
               partial_match
               (G.NodeSet.union new_succs_q new_preds_q)
               (acc2, new_matched_q, new_succs_q, new_preds_q)
                                 
                                            
                          
      
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

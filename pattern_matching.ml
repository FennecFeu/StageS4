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

    module Matching =
      struct
        type t = G.NodeType.t NodeMap.t
        let compare = NodeMap.compare G.NodeType.compare
      end

    module ResultSet = Set.Make(Matching)

    type state =
      { matching_peers : Matching.t;
        matched_g : G.NodeSet.t;
        succs_g : G.NodeSet.t;
        preds_g : G.NodeSet.t
      }

    (*
      @requires None
      @ensures Returns the key corresponding to the value in the map
      @raises failwith if the value is not registered in the map
     *)
    let reverse_find value map =
      match NodeMap.fold (fun k v acc -> if v = value then Some k else acc) map None with
      | Some k -> k
      | None -> failwith "This case is not going to happen"
    
    (*
      @requires None
      @ensures Returns a set of all matchings between q and a subgraph of g where q and the subgraph have the same structure
     *)
    let find_matched_pattern q g =

      (*
        @requires None
        @ensures Returns true if the node n is compatible with m. If not, returns false
       *)
      let is_compatible_node m n =
        true (* Temporairement *)
      in

      (*
        @requires None
        @ensures Returns true if the edge between n and n' is compatible with the edge between m and m'. If not, returns false
       *)
      let is_compatible_edge m m' n n' =
        true (* Temporairement *)
      in

      (*
        @requires matched_q is the set of all the keys in s.matching_peers. succs_q and preds_q are all the successors and predecessors of matched_q nodes in the graph q, excluding the nodes in matched_q. m does not belong to those sets. The same goes for the elements in the state s.
        @ensures Returns true if m and n satisfy all the criteria of the filter. Returns false if at least one criterion is not satisfied
       *)
      let fsyn s matched_q succs_q preds_q m n =
        (* VF2 feasability rule for node predecessors cardinality *)
        let matched_preds_m = G.NodeSet.inter matched_q (G.preds q m) in
        let matched_preds_n = G.NodeSet.inter s.matched_g (G.preds g n) in
        G.NodeSet.for_all
          (fun m' -> let n' = NodeMap.find m' s.matched m' s.matching_peers in
                     G.NodeSet.mem n' matched_preds_n
                     && is_compatible_edge m m' n n')
          matching_preds_m
        &&
          G.NodeSet.for_all
            (fun n' -> let m' = reverse_find n' s.matching_peers in
                       G.NodeSet.mem m' matched_preds_m
                       && is_compatible_edge m m' n n')
            matching_preds_n
        &&
          (* VF2 feasibility rule for node successors cardinality *)
          let matched_succs_m = G.NodeSet.inter matched_q (G.succs q m) in
          let matched_succs_n = G.NodeSet.inter s.matched_g (G.succs g n) in
          G.NodeSet.for_all
            (fun m' -> let n' = NodeMap.find m' s.matching_peers in
                       G.NodeSet.mem n' matched_succs_n
                       && is_compatible_edge m m' n n')
            matching_succs_m
          &&
            G.NodeSet.for_all
              (fun n' -> let m' = reverse_find n' s.matching_peers in
                         G.NodeSet.mem m' matched_succs_m
                         && is_compatible_edge m m' n n')
              matching_succs_n
          
          &&
            (* VF2 feasibility rule for node predecessors cardinality with 1-look-ahead *)
            let neighbours_preds_m = G.NodeSet.inter preds_q (G.preds q m) in
            let neighbours_preds_n = G.NodeSet.inter s.preds_g (G.preds g n) in
            G.NodeSet.cardinal neighbours_preds_m <= G.NodeSet.cardinal neighbours_preds_n
            &&
              (* VF2 feasibility rule for node successors cardinality with 1-look-ahead *)
              let neighbours_succs_m = G.NodeSet.inter succs_q (G.succs q m) in
              let neighbours_succs_n = G.NodeSet.inter s.succs_g (G.succs g n) in
              G.NodeSet.cardinal neighbours_succs_m <= G.NodeSet.cardinal neighbours_succs_n
      in

      (*
        @requires m does not belong to the keys in s.matching_peers
        @ensures add the peering m -> n in s, and apply all the consequences on others elements in s to make a new consistent state.
       *)
      let add_peering m n s =
        let new_matching_peers = NodeMap.add m n s.matching_peers in
        let new_matched_g = G.NodeSet.add n s.matched_g in
        let new_succs_g = G.NodeSet.union s.succs_g (G.NodeSet.diff (G.succs g n) new_matched_g) in
        let new_preds_g = G.NodeSet.union s.preds_g (G.NodeSet.diff (G.preds g n) new_matched_g) in
        { matching_peers = new_matching_peers;
          matched_g = new_matched_g;
          succs_g = new_succs_g;
          preds_g = new_preds_g;
        }
      in
        
        
      (*
        @requires ls is a valid list of all partial mappings between the two graphs; succs_q and preds_q are all the successors and predecessors of nodes in matched_q that not belong to matched_q; m in q
        @ensures Returns the set of all mappings between the two graphs until this branch
       *)
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
                            else let new_s = add_peering m n tmp_s in
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
      in
                                 
                                            
                          
      
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

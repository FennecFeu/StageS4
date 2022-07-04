module type FindMatchedPattern =
  sig

    module GType : Graph.Graph

    module Matching : Set.OrderedType

    module ResultSet : Set.S with type elt = Matching.t

    val find_matched_pattern : GType.graph -> GType.graph -> ResultSet.t

    val print_result : Matching.t -> unit
  end
  

module MakeFindMatchedPattern(G:Graph.Graph) =
  struct

    module GType = G

    (* NodeMap is a module created to build a map with nodes as keys and values *)
    module NodeMap = Map.Make(G.NodeType)

    (* Matching is a module created to allow making a set of matchings *)
    module Matching =
      struct
        type t = G.NodeType.t NodeMap.t
        let compare = NodeMap.compare G.NodeType.compare
      end

    (* ResultSet is a module created to allow making a set of sets of matchings *)
    module ResultSet = Set.Make(Matching)

    (* qstate is a record which stocks the state of the research in the query graph while the program is running *)
    type qstate =
      { n_seen : G.NodeSet.t;
        n_seen_succ : G.NodeSet.t;
        n_seen_pred : G.NodeSet.t;
      }

    (* gstate is a record which stocks the state of the research in the data graph while the program is running *)
    type gstate =
      { matching_peers : Matching.t;
        a_seen : G.NodeSet.t;
        a_seen_succ : G.NodeSet.t;
        a_seen_pred : G.NodeSet.t;
        a_next : G.NodeSet.t;
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
        @ensures Returns true if the atom a is compatible with the node n. If not, returns false
       *)
      let is_compatible_node n a =
        true (* Temporarily *)
      in

      (*
        @requires None
        @ensures Returns true if the edge between n and n' is compatible with the edge between m and m'. If not, returns false
       *)
      let is_compatible_edge n n' a a' =
        true (* Temporarily *)
      in

      (*
        @requires m' is a key in mps
        @ensures Returns false if the criterion must eliminate the entry, if not returns true
       *)
      let nearby_matched_filter mps ens n a n' =
        let a' = NodeMap.find n' mps in
        G.NodeSet.mem a' ens && is_compatible_edge n n' a a'
      in

      (*
        @requires matched_q is the set of all the keys in s.matching_peers. succs_q and preds_q are all the successors and predecessors of matched_q nodes in the graph q, excluding the nodes in matched_q. m does not belong to those sets. The same goes for the elements in the state s.
        @ensures Returns true if m and n satisfy all the criteria of the filter. Returns false if at least one criterion is not satisfied
       *)
      let is_valid g_state q_state n a =
        
        (* VF2 feasability rule for node predecessors cardinality *)
        let matched_preds_n = G.NodeSet.inter q_state.n_seen (G.preds q n) in
        let matched_preds_a = G.NodeSet.inter g_state.a_seen (G.preds g a) in
        G.NodeSet.for_all
          (nearby_matched_filter g_state.matching_peers matched_preds_a n a)
          matched_preds_n
        (**
        &&
          G.NodeSet.for_all
            (fun a' -> let n' = reverse_find a' g_state.matching_peers in
                       G.NodeSet.mem n' matched_preds_n
                       && is_compatible_edge n n' a a')
            matched_preds_a
         **)
        &&
          (* VF2 feasibility rule for node successors cardinality *)
          let matched_succs_n = G.NodeSet.inter q_state.n_seen (G.succs q n) in
          let matched_succs_a = G.NodeSet.inter g_state.a_seen (G.succs g a) in
          G.NodeSet.for_all
            (nearby_matched_filter g_state.matching_peers matched_succs_a n a)
            matched_succs_n
          (**
          &&
            G.NodeSet.for_all
              (fun a' -> let n' = reverse_find a' g_state.matching_peers in
                         G.NodeSet.mem n' matched_succs_n
                         && is_compatible_edge n n' a a')
              matched_succs_a
            **)
          
          &&
            (* VF2 feasibility rule for node predecessors cardinality with 1-look-ahead *)
            let neighbours_preds_n = G.NodeSet.inter q_state.n_seen_pred (G.preds q n) in
            let neighbours_preds_a = G.NodeSet.inter g_state.a_seen_pred (G.preds g a) in
            G.NodeSet.cardinal neighbours_preds_n <= G.NodeSet.cardinal neighbours_preds_a
            &&
              (* VF2 feasibility rule for node successors cardinality with 1-look-ahead *)
              let neighbours_succs_n = G.NodeSet.inter q_state.n_seen_succ (G.succs q n) in
              let neighbours_succs_a = G.NodeSet.inter g_state.a_seen_succ (G.succs g a) in
              G.NodeSet.cardinal neighbours_succs_n <= G.NodeSet.cardinal neighbours_succs_a
      in

      (*
        @requires n does not belong to the keys in s.matching_peers
        @ensures add the peering n -> a in s, and apply all the consequences on others elements in s to make a new consistent state.
       *)
      let add_peering_g n a s =
        let new_matching_peers = NodeMap.add n a s.matching_peers in
        let new_a_seen = G.NodeSet.add a s.a_seen in
        let new_a_seen_succ = G.NodeSet.union s.a_seen_succ (G.NodeSet.diff (G.succs g a) s.a_seen) in
        let new_a_seen_pred = G.NodeSet.union s.a_seen_pred (G.NodeSet.diff (G.preds g a) s.a_seen) in
        { matching_peers = new_matching_peers;
          a_seen = new_a_seen;
          a_seen_succ = new_a_seen_succ;
          a_seen_pred = new_a_seen_pred;
          a_next = G.NodeSet.union (G.succs g a) (G.preds g a)
        }
      in

      (*
        @requires s is a consistent state except n has already been removed in n_seen_succ and n_seen_pred
        @ensures add the node n to the state s. The operation is called "add_peering" because it comes in addition with the operation "add_peering_g" to stay consistent overall
       *)
      let add_peering_q n s =
        let new_n_seen = G.NodeSet.add n s.n_seen in
        let new_n_seen_succ = G.NodeSet.union s.n_seen_succ (G.NodeSet.diff (G.succs q n) new_n_seen) in
        let new_n_seen_pred = G.NodeSet.union s.n_seen_pred (G.NodeSet.diff (G.preds q n) new_n_seen) in
        { n_seen = new_n_seen;
          n_seen_succ = new_n_seen_succ;
          n_seen_pred = new_n_seen_pred;
        }
      in
      

      (*
        @requires None
        @ensures Returns q_state but the last two fields of the record get n removed from the set.
       *)
      let remove_adj n q_state =
        {q_state with n_seen_succ = G.NodeSet.remove n q_state.n_seen_succ;
                      n_seen_pred = G.NodeSet.remove n q_state.n_seen_pred;
        }
      in
      
        
        
      (*
        @requires g_states is a valid list of all partial mappings between the two graphs; q_state.n_seen_succ and q_state.n_seen_pred are all the successors and predecessors of nodes in q_state.n_seen that not belong to q_state.n_seen; n in q
        @ensures Returns the set of all mappings between the two graphs until this branch
       *)
      let rec partial_match n (g_states, q_state) =
        if (g_states = []) || (G.NodeSet.mem n q_state.n_seen) then (g_states, q_state)
        else let upd_q_state = remove_adj n q_state in
             let new_g_states =
               List.fold_left
                 (fun acc g_state ->
                   G.NodeSet.fold
                     (fun a aux_acc ->
                       if not (is_compatible_node n a) then aux_acc
                       else let tmp_g_state = {g_state with a_seen_succ = G.NodeSet.remove a g_state.a_seen_succ;
                                                            a_seen_pred = G.NodeSet.remove a g_state.a_seen_pred;
                                              } in
                            if not (is_valid tmp_g_state upd_q_state n a) then aux_acc
                            else let new_g_state = add_peering_g n a tmp_g_state in
                                 new_g_state::aux_acc)
                     g_state.a_next acc)
                 [] g_states in

             let new_q_state = add_peering_q n upd_q_state in
                 
             G.NodeSet.fold
               partial_match
               (G.NodeSet.union (G.succs q n) (G.preds q n)) (* = N_new_next *)
               (new_g_states, new_q_state)
      in
                                 
                                            
                          
      (* Here is the initialisation of the algorithm *)
      
      if G.is_empty q then ResultSet.empty
      else let n = G.get_min_node q in
           (* n is the minimum according to its ordered type *)
           let all_starts =
             G.fold_node (fun acc a -> if not (is_compatible_node n a) then acc
                                       else ResultSet.add (NodeMap.add n a NodeMap.empty) acc)
               ResultSet.empty g in
           (* all_starts related to all the first peerings that can be made with the node n *)
           
           let g_states =
             ResultSet.fold
               (fun mp acc -> let a = NodeMap.find n mp in
                              let a_seen_succ = G.NodeSet.remove a (G.succs g a) in
                              let a_seen_pred = G.NodeSet.remove a (G.preds g a) in
                              let new_g_state =
                                { matching_peers = mp;
                                  a_seen = G.NodeSet.add a G.NodeSet.empty;
                                  a_seen_succ = a_seen_succ;
                                  a_seen_pred = a_seen_pred;
                                  a_next = G.NodeSet.union a_seen_pred a_seen_succ;
                                } in
                              new_g_state::acc)
               all_starts [] in

           let q_state =
             { n_seen = G.NodeSet.add n G.NodeSet.empty;
               n_seen_succ = G.NodeSet.remove n (G.succs q n);
               n_seen_pred = G.NodeSet.remove n (G.preds q n);
             } in
           let (new_g_states, _) = G.NodeSet.fold partial_match (G.NodeSet.union q_state.n_seen_succ q_state.n_seen_pred) (g_states, q_state) in
           List.fold_left (fun acc state -> ResultSet.add state.matching_peers acc) ResultSet.empty new_g_states

    let print_association k v =
      let _ = G.print_node k in
      let _ = Printf.printf " -> " in
      let _ = G.print_node v in
      Printf.printf " ; "

    let print_matching m =
      let _ = NodeMap.iter print_association m in
      Printf.printf "\n\n"

    let print_result result =
      let _ = Printf.printf "Nombre de résultats : %i\n" (ResultSet.cardinal result) in
      ResultSet.iter print_matching result
                                              
  end

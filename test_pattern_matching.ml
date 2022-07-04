open Edge
open Graph
open Pattern_matching

module Int =
  struct
    type t = int
    let compare a b = a-b
    let print_t a = Printf.printf "%i" a
  end

module EdgeInt = MakeEdge(Int)
module GraphInt = MakeGraph(EdgeInt)
module FMPInt = MakeFindMatchedPattern(GraphInt)

let edgeq1 = EdgeInt.edge 1 2
let edgeq2 = EdgeInt.edge 2 3
let edgeq3 = EdgeInt.edge 3 1
let edgeq4 = EdgeInt.edge 4 1
              
let q = FMPInt.GType.add_edge edgeq4
          (FMPInt.GType.add_edge edgeq3
             (FMPInt.GType.add_edge edgeq2
                (FMPInt.GType.add_edge edgeq1
                   FMPInt.GType.empty
          )))

let edgeg1 = EdgeInt.edge 1 2
let edgeg2 = EdgeInt.edge 1 4
let edgeg3 = EdgeInt.edge 2 3
let edgeg4 = EdgeInt.edge 3 1
let edgeg5 = EdgeInt.edge 3 4
let edgeg6 = EdgeInt.edge 3 5
let edgeg7 = EdgeInt.edge 4 2
let edgeg8 = EdgeInt.edge 4 5
      
let g = FMPInt.GType.add_edge edgeg8
          (FMPInt.GType.add_edge edgeg7
             (FMPInt.GType.add_edge edgeg6
                (FMPInt.GType.add_edge edgeg5
                   (FMPInt.GType.add_edge edgeg4
                      (FMPInt.GType.add_edge edgeg3
                         (FMPInt.GType.add_edge edgeg2
                            (FMPInt.GType.add_edge edgeg1
                               FMPInt.GType.empty
          )))))))

let result = FMPInt.find_matched_pattern q g;;

FMPInt.print_result result;;

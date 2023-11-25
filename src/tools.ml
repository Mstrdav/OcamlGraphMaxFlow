(* Yes, we have to repeat open Graph. *)
open Graph

(* clone_nodes gr
 returns a new graph having the same nodes than gr, but no arc. *)
let clone_nodes gr = n_fold gr new_node empty_graph ;;

(* 
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
returns a new graph where all the edges e of gr have been replaced by
the result of the application of f to e. *)
let gmap gr f = e_fold gr (fun g {src=id1 ; tgt=id2 ; lbl=lbl} -> new_arc g {src=id1 ; tgt=id2 ; lbl=(f lbl)}) (clone_nodes gr);;

(* let gmap_double gr1 gr2 f = e_fold_double gr1 gr2 (fun g {src=id1 ; tgt=id2 ; lbl=lbl1} {src=id3 ; tgt=id4 ; lbl=lbl2} -> new_arc g {src=id1 ; tgt=id3 ; lbl=(f lbl1 lbl2)}) (clone_nodes gr1) (clone_nodes gr2);; *)


(* add_arc g id1 id2 n
 adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created. *)
let add_arc g id1 id2 n = match find_arc g id1 id2 with
  | None -> new_arc g {src=id1 ; tgt=id2 ; lbl=n}
  | Some arc -> new_arc g {src=id1 ; tgt=id2 ; lbl=arc.lbl + n};;
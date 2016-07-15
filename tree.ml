(* Adapted from code handed out in class by Professor John Hale*)

type tree = Leaf of string | Branch of string * tree list

let unary label child = Branch (label,[child])
let binary label (child1,child2) = Branch (label,[child1;child2])
let ternary label ((child1,child2),child3) = Branch (label,[child1;child2;child3])
let fourary label (((child1,child2),child3),child4) = Branch (label,[child1;child2;child3;child4])
let multiple label kids = Branch (label,kids)


(* tree writer for the AT&T dot format *)
let dot_of_tree title t =
  let rec dot_of_node i = function
      Leaf name ->  (("n"^(string_of_int i)^" [label = \""^name^"\"];\n"),i)
    | Branch (parent,kids) ->
        let (rootbyindexlist,maximum) = List.fold_left (fun (sofar,index) kid ->
          let (result,newindex) = dot_of_node (index+1) kid in
          ((result,(index+1))::sofar,newindex)
                                        )
            ([],i)
            kids in
        let thisnode = ("n"^(string_of_int i)^" [label = \""^parent^"\"];\n") in
        let downarrows = List.fold_left (fun already (subtree,index) ->
          ("n"^(string_of_int i)^"-> n"^(string_of_int index)^";\n"^already)
                        )
            ""
            rootbyindexlist in
        let subtreedot = List.fold_left (fun already (subtree,index) ->
                                             subtree^already)
            ""
            rootbyindexlist in
        (thisnode^downarrows^subtreedot,maximum)

 in
  ("digraph \""^title^"\" {\n node [shape = plaintext]; \n edge [arrowhead = none]; \n"^(Pervasives.fst (dot_of_node 0 t))^"}")


let writetree name t =
  let oc = open_out name in
  output_string oc (dot_of_tree name t);
  close_out oc;;

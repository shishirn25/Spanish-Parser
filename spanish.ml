   #use "tree.ml";;
   #use "utilities.ml";; 





(* types from september 10th
   this type says that a combinatorparser is a function from remainders to
   lists of remainders, each of which is paired up with a value of an undetermined type 'a
*)
type remainder = string list
type 'a combinatorparser = remainder -> ('a * remainder) list

(* 1. a parser that recognizes individual words, as specified on the list l *)
let (any_one_of : string list -> tree combinatorparser) l = function
    x::xs when (List.mem x l) -> [((Leaf x),xs)]
    | _ -> []

(* 2. alternative. both disjuncts should return the same type of result *)
let ( |. ) (p : 'a combinatorparser) (q : 'a combinatorparser) xs = List.append (p xs) (q xs)

(* 3. sequencing defined with a list comprehension: *)
(* feed the output of p into q. pair up the results *)
let ( &. ) (p : 'a combinatorparser) (q : 'b combinatorparser) xs =
  [? List:((r1,r2),zs) | (r1,ys) <- List:(p xs); (r2,zs) <- List:(q ys) ?]

(* 4. "gives". apply f to all the results of combinatorparser p *)
let ( >. ) (p : 'a combinatorparser) f xs =
  [? List:(f x,ys) | (x,ys) <- List:(p xs) ?]




let wrapper (p : tree combinatorparser) words = 
  let finished analysis = match analysis with
      (_,[]) -> true
    | _ -> false  in
  List.map Pervasives.fst (List.filter finished
           (p (split_on [' '] words)))







let (empty : tree combinatorparser) words = [((Leaf "0"),words)] (* always succeeds *)
let opt p = p |. empty

let rec s words = ((np &. vp >. binary "S")
         |. (np &. subjVP >. binary "S")) words
  and np words = ((def_article &. pronoun >. binary "NP")
         |. (pronoun >. unary "NP")
         |. (n_object >. unary "NP")
         |. (indef_art &. n_person >. binary "NP")
         |. (def_article &. n_object >. binary "NP")
         |. (rel_clause >. unary "NP")
         |. (command >. unary "command")
           ) words
  and subjNP words = ((indef_art &. n_person &. subj_rel_clause >. ternary "SubjNP")
         |. (subj_rel_clause &. (opt np) >. binary "SubjNP")
           ) words

  and vp words = ((ind_comm_pronoun &. inf_v >. binary "indCommand")
         |. (vintrans >. unary "VP")
         |. ((opt neg) &. vtrans &. other_adv_clause >. ternary "other_adv_clause")
         |. ((opt reflex) &. (opt pronoun) &. vtrans >. ternary "VP")
         |. ((opt neg) &. vtrans &. np >. ternary "VP")
         |. ((opt neg) &. vtrans &. temp_adv_clause &. rel_clause >. fourary "VP")
         |. ((opt neg) &. vintrans &. temp_adv_clause &. rel_clause >. fourary "VP")
         |. (neg &. subj_n_clause_trigger &. rel_clause >. ternary "VP")
           ) words
  and subjVP words = (((opt neg) &. vtrans &. subjNP >. ternary "subjVP")
         |. ((opt neg) &. vtrans &. adv_clause >. ternary "subjVP")
         |. (subj_n_clause_trigger &. subjNP >. binary "subjVP")
         |. (futV &. temp_adv_clause &. subj_rel_clause >. ternary "subjVP")
         |. (vintrans &. as_if_clause >. binary "subjVP")
         |. (subjV >. unary "subjV")
         |. (pronoun &. (opt neg) &. inf_v >. ternary "subjComand")
         |. (ind_comm_pronoun &. neg &. subj_inf_v >. ternary "subjCommand")
  ) words


  and vtrans words  = (any_one_of ["necesito";"quiero";"hizo";"se";"voy"] >. unary "Vtrans") words (* I need, I want, he did, I know, I go*)
  and vintrans words = (any_one_of["corre";"camina"] >. unary "vintrans") words (*he runs, he walks*)
  and futV words = (any_one_of["correre"] >. unary "future V") words(*He will run*)
  and inf_v words = (any_one_of["hablar";"bailar"] >. unary "inf_v") words (*to talk, to dance*)
  and subj_inf_v words = (any_one_of["hablar";"bailar"] >. unary "subj_inf_v") words(*to talk, to dance*)
  and subjV words  = (any_one_of ["insulta";"hacia";"va"] >. unary "subjV") words (*he insults, he made, he goes*)

  and n_person words = (any_one_of ["amigo"] >. unary "n_person") words (*friend*)
  and n_object words = (any_one_of ["tarea";"tiempo"] >. unary "n Object") words (*homework, time*)
  and def_article words = (any_one_of ["la"] >. unary "D") words (*the*)
  and indef_art words = (any_one_of ["ningun";"un"] >. unary "Indefinite Article") words(*no one, one/a*)
  and subj_rel_clause words = ( (any_one_of["que"]) &. (opt pronoun) &. (opt reflex) &. subjV >. fourary "subj_rel_clause") words (*that*)
  and rel_clause words = ((any_one_of["que"]) &. (opt pronoun) &. vp >. ternary "rel_clause") words

  and adv_clause words = ((opt prep) &. any_one_of["antes";"para";"fin";"sin";"menos"] &. (opt prep) &. subj_rel_clause >. fourary "adv_clause") words (*before, so that, until, without, unless*)
  and temp_adv_clause words = (any_one_of["hasta";"luego"] >. unary "temp_adv_clause") words (*until, as soon as *)
  and other_adv_clause words = (((any_one_of["aunque"]) &. (opt pronoun) &. (opt reflex) &.subjV >. fourary "other_adv_clause") (*althouugh*)
  |. ((any_one_of["aunque"]) &. (opt pronoun) &. (opt reflex) &. vintrans >. fourary "other_adv_clause"))words
  and as_if_clause words = (as_if &. (opt (any_one_of["no"])) &. vPastSubj >. ternary "as_if_clause") words(*no*)

  and vPastSubj words = (any_one_of["habia"] &. n_object >. binary "vPastSubj") words(*there was/there were*)
  and as_if words = (any_one_of["como"] &. any_one_of["si"] >. binary "as if") words(*as if*)

  and prep words = (any_one_of["a";"de"] >. unary "prep") words (*to, from*)
  
  and subj_n_clause_trigger words = (any_one_of["espero";"dudo";"deseo"] >. unary "subj_n_clause_trigger") words (*I expect, I doubt, I desire*)
  and reflex words = (any_one_of["me"] >. unary "reflex") words (*me*)
  and pronoun words = (any_one_of ["yo";"ella";"nosotros"] >. unary "pronoun") words (*I, she, us*)
  and ind_comm_pronoun words = (any_one_of["tu";"vosotros"] >. unary "ind_comm_pronoun") words (*you, you plural*)

  
  and command words = (any_one_of["mandato"] >. unary "command") words (*command*)
  and neg words = (any_one_of ["no"] >. unary "neg") words(*no, dont*)
  

  (*Test Cases
  wrapper s "Yo no necesito ningun amigo que me insulta"
  - I don't need a friend that insults me.
  wrapper s "mandato tu hablar"
  -command talk  (2nd person singular)
  wrapper s "mandato tu no hablar"
  -command don't talk (2nd person singular)
  wrapper s "yo no voy a menos que ella va"
  -I won't go unless she goes.
  wrapper s "yo espero que ella va"
  -I expect that she will go 
  wrapper s "yo no dudo que ella corre"
  -I don't doubt that she runs 
  wrapper s "yo correre hasta que ella va"
  -I will run intil she goes.
  wrapper s "ella corre como si no habia tiempo"
  -She runs as if there were no time.

  )
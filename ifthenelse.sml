     (* These functions are needed for if-then-else expressions and functions *)
     val label = ref 0;

     fun nextLabel() = 
         let val lab = !label
         in 
           label := !label + 1;
           "L"^Int.toString(lab)
         end

     val relOpOpposites = [("=","<>"),("<>","="),("<=",">"),(">=","<"),("<",">="),(">","<=")];

     exception notLocated;
   
     fun opposite(relOp) = 
       let fun mappedVal x nil = raise notLocated
             | mappedVal (x:string) ((y,z)::rest) = if x = y then z else mappedVal x rest
       in
         mappedVal relOp relOpOpposites 
       end;

"opposite of >";
opposite(">");
"next label";
nextLabel();
"next label";
nextLabel();

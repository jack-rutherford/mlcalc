     exception unboundId;  
     
     datatype Type = function' of string
                   | constant' of string;
      
     fun boundTo(name,[]) = 
         let val idname = (case name of 
                              function'(s) => s
                            | constant'(s) => s)
         in
           TextIO.output(TextIO.stdOut, "Unbound identifier "^idname^" referenced or type error!\n");
           raise unboundId
         end

       | boundTo(name,(n,ol,depth)::t) = if name=n then ol else boundTo(name,t);

"binding function f to label L1";
boundTo(function'("f"),[(function'("f"),"L1",0)]);

"example6.12 binding for x";
boundTo(constant'("x"), [(constant'("x"),"0",0)]);

"practice6.6 binding for y and x";
boundTo(constant'("y"), [(constant'("x"),"2",0),(constant'("y"),"1",0),(constant'("x"),"0",0)]);
boundTo(constant'("x"), [(constant'("x"),"2",0),(constant'("y"),"1",0),(constant'("x"),"0",0)]);

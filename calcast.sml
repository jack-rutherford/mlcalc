structure calcAS = 
struct

datatype
    AST = add' of AST * AST
        | sub' of AST * AST
        | prod' of AST * AST
        | div' of AST * AST
        | negate' of AST
        | integer' of int
        | store' of AST
        | recall'
        | get'
        | letval' of string * AST * AST
        | valref' of string;
	   
fun show(valref'(id)) = "valref("^id^")"
  | show(letval'(id,a,b)) = "letval("^id^","^show(a)^","^show(b)^")"
  | show(get') = "get" 
  | show(recall') = "recall"
  | show(integer'(i)) = "integer("^Int.toString(i)^")"
  | show(store'(a)) = "store("^show(a)^")"
  | show(negate'(a)) = "negate("^show(a)^")"
  | show(div'(a,b)) = "div("^show(a)^","^show(b)^")"
  | show(prod'(a,b)) = "prod("^show(a)^","^show(b)^")"
  | show(sub'(a,b)) = "sub("^show(a)^","^show(b)^")"
  | show(add'(a,b)) = "add("^show(a)^","^show(b)^")"

end;



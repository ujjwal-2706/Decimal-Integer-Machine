val filename = TextIO.openIn "abs.bdim";
val read1 = Option.valOf(TextIO.inputLine(filename));
size(read1);
val maxMemSize = 100;
val mem = Array.array(maxMemSize,~1);
val x = valOf(Int.fromString("33433"));

(*Made a funciton to parse a single line and check if it has valid syntax*)
exception SyntaxError;
fun isdigit("0") = true|isdigit("1") = true| isdigit("2") = true|isdigit("3")= true
    |isdigit("4") = true|isdigit("5") = true| isdigit("6") = true|isdigit("7") = true
    |isdigit("8") = true|isdigit("9") = true|isdigit("(") = true|isdigit(")") = true
    |isdigit(",") = true|isdigit("-") = true|isdigit(other) = false;
fun isValid(string_value) =let fun ifStatementValid(string_value,i)= if i>= size(string_value)-1
     then true else 
     (if isdigit(String.substring(string_value,i,1)) then ifStatementValid(string_value,i+1)
     else  raise SyntaxError) in ifStatementValid(string_value,0) end;
fun special(",") = true|special("(") = true| special(")")=  true|special(character)= false;
(*special function for checking for special character*)


(*function to make array of length 4 using strings assuming valid syntax*)
fun singleNum(string_value,i) = if special(String.substring(string_value,i,1)) then 
        (let fun go(j) = if  special(String.substring(string_value,j,1)) then
        (let val nums = valOf(Int.fromString(String.substring(string_value,i+1,j-i-1)));
            val index = j;
            val newArray = Array.array(2,~1);
            val first = Array.update(newArray,0,nums);
            val second = Array.update(newArray,1,index);
            in newArray end
        ) 
        else go(j+1)
        in go(i+1) end) else raise SyntaxError;


fun quadruple(string_value,i,arr,j) = if i >= size(string_value)-2 then arr else 
        (let val number = Array.sub(singleNum(string_value,i),0);
            val index = Array.sub(singleNum(string_value,i),1);  
            val first =  Array.update(arr,j,number); in
        quadruple(string_value,index,arr,j+1) end);
fun makeQuad(string_value) =let val arr = Array.array(4,~1) in quadruple(string_value,0,arr,0) end;
makeQuad(read1); 
val read1 = Option.valOf(TextIO.inputLine(filename));
makeQuad(read1);

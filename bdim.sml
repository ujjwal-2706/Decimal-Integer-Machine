val filename = TextIO.openIn "abs.bdim";
(* val read1 = Option.valOf(TextIO.inputLine(filename)); *)
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
(* val what = makeQuad(read1); *)

(*-----Function makeQuad will read the string line of .bdim and give a corresponding quadruple
 array as a result which we will use to generate our vector code of instruction----*)

 

fun string_vector(file,list) = let val string_option = TextIO.inputLine(file);
        in (if string_option = NONE then list else 
            string_vector(file,list @[Option.valOf(string_option)]) ) end;


fun code(file) = let val string_list = string_vector(file,[]) ;
                    val generate = map makeQuad;
                in map makeQuad string_list end; 
(*The function code will take input the bdim file and give output as a list of 4 element
array which we will traverse for the instruction*)
(*output type of code is int array list*)


(*---------Now we shall make the function for individual instruction-------*)

fun op2(array) = let val i = Array.sub(array,1);
                     val k = Array.sub(array,3);
                 in Array.update(mem,k,Array.sub(mem,i)) end;
fun op3(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val k = Array.sub(array,3);
                in (if mem_i =1 then Array.update(mem,k,0) else 
                    (if mem_i = 0 then Array.update(mem,k,1)else 
                    raise Subscript)) end;
fun op4(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                    fun temp(0,0) = Array.update(mem,k,0)
                    |temp(1,1) = Array.update(mem,k,1)
                    |temp(1,0) = Array.update(mem,k,1)
                    |temp(0,1) = Array.update(mem,k,1)
                    |temp(x,y) = raise Subscript;
                    in temp(mem_i,mem_j) end;
fun op5(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                    fun temp(1,1) = Array.update(mem,k,1)
                    |temp(0,1) = Array.update(mem,k,0)
                    |temp(1,0) = Array.update(mem,k,0)
                    |temp(0,0) = Array.update(mem,k,0)
                    |temp(x,y) = raise Subscript;
                    in temp(mem_i,mem_j) end;
fun op6(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                in Array.update(mem,k,mem_i + mem_j) end;
fun op7(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                in Array.update(mem,k,mem_i - mem_j) end;
fun op8(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                in Array.update(mem,k,mem_i * mem_j) end;
fun op9(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                in Array.update(mem,k,mem_i div mem_j) end;
fun op10(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                in Array.update(mem,k,mem_i mod mem_j) end;
fun op11(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                in (if mem_i = mem_j then Array.update(mem,k,1)
                else Array.update(mem,k,0)) end;
fun op12(array) = let val mem_i = Array.sub(mem,Array.sub(array,1));
                    val mem_j = Array.sub(mem,Array.sub(array,2));
                    val k = Array.sub(array,3);
                in (if mem_i > mem_j then Array.update(mem,k,1)
                else Array.update(mem,k,0)) end;
fun op15(array) = print(Int.toString(Array.sub(mem,Array.sub(array,1))));
fun op16(array) = let val v = Array.sub(array,1);
                    val k = Array.sub(array,3);
                in Array.update(mem,k,v) end;

fun op1(array) = let val str = valOf(TextIO.inputLine TextIO.stdIn);
                    val v = valOf (Int.fromString str);
                    val k = Array.sub(array,3);
                in Array.update(mem,k,v) end;

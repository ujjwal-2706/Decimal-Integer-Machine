val filename = TextIO.openIn "abs.bdim";
val read1 = Option.valOf(TextIO.inputLine(filename));
size(read1);
val maxMemSize = 100;
val mem = Array.array(maxMemSize,~1);

exception SyntaxError;
fun isdigit("0") = true|isdigit("1") = true| isdigit("2") = true|isdigit("3")= true|isdigit("4") = true|isdigit("5") = true| isdigit("6") = true|isdigit("7") = true|isdigit("8") = true|isdigit("9") = true|isdigit("(") = true|isdigit(")") = true|isdigit(",") = true|isdigit(other) = false;
fun isValid(string_value) = let fun ifStatementValid(string_value,i) = if i>= size(string_value) then true else (if isdigit(String.substring(string_value,i,1)) then ifStatementValid(string_value,i+1) else  raise SyntaxError) in ifStatementValid(string_value,0) end;

(* val file = TextIO.openIn("ap.bdim");
val read1 = TextIO.inputLine(file);
(* val read2 = (StreamIO.endOfStream(file)); *)
(* read2 = NONE; *)
(* val file2 = StreamIO.reader("ap.bdim"); *)
val vec1 = Vector.fromList([Array.array(2,9)]);
val vec2 = Vector.fromList([Array.array(3,1)]);
val vec1=  Vector.concat([vec1,vec2]); *)
fun getNumber() = (
print "Please enter the number of integers: ";
let
    val str = valOf (TextIO.inputLine TextIO.stdIn)
    val i : int = valOf (Int.fromString str)
in
    i
end
);
val k = getNumber();

(* val k = TextIO.inputLine TextIO.stdIn; *)
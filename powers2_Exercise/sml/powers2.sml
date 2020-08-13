(* reverses a list in an efficient way *)
fun reverse xs =
   let
        fun rev (nil, z) = z
        | rev (y::ys, z) = rev (ys, y::z)
    in
        rev (xs, nil)
    end;

(* Input parse code by Stavros Aronis, modified by Nick Korasidis. *)
fun parse file =
    let
	(* A function to read an integer from specified input. *)
        fun readInt input =
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

	(* Open input file. *)
    	val inStream = TextIO.openIn file

        (* Read an integer (number of countries) and consume newline. *)
	val n = readInt inStream
	val _ = TextIO.inputLine inStream

(* A function to read N integers from the open file. *)
	fun readInts 0 acc = reverse acc (* Replace with 'rev acc' for proper order. *)
	  | readInts i acc = readInts (i - 1) (readInt inStream :: acc)
    in
   	(n, readInts (2*n) [])
    end


(* prints the list in the way we need it *)
fun print_list l length =
        if(length = 0) then print("]\n") else (
                if((length = 1)) then print(Int.toString(hd l) ^ "]\n")
                else (print(Int.toString(hd l) ^ ","); print_list (tl l) (length-1)))


(* converts a decimal number to a binary *)
fun decimal_to_binary (number) =
    let
        fun whileLoop n lis k =
            if (n = 0) then (lis, k)
            else (if ((n mod 2) = 1) then (whileLoop (n div 2) ((n mod 2)::lis) (k+1))
                                     else (whileLoop (n div 2) ((n mod 2)::lis) k))
    in
        whileLoop number nil 0
    end

(* This is the algorithm that was used in the same problem but in c++
*  an here we just tried to "translate" it into sml*)
fun algorithm l k count =
    let
        fun forLoop1 j j_stop l1 =
           let
             fun forLoop2 x (plinEna::tora::ypoloipoList) l_length =
                if((tora > 0)  andalso (x < (l_length-1)))
                        then (plinEna + 2)::(tora-1)::ypoloipoList
                        else (if (((tora) = 1) andalso (x = (l_length-1)))
                                then [plinEna+2]
                                else ( if(tora>1 andalso x = (l_length-1))
                                        then (plinEna+2)::(tora-1)::ypoloipoList
                                        else (plinEna::  (forLoop2 (x+1) (tora::ypoloipoList) l_length))
                                )
                             )
             |   forLoop2 x randomLis l_length =
                        (print("sth wrong in forLoop2: "^ Int.toString(x) ^ " "
                        ^ Int.toString(l_length)^ ": "); (print("[");
                        (print_list randomLis (length randomLis))); nil)
            in
              (
              if(j>=j_stop) then l1
                            else (
                                let
                                  val lis = forLoop2 1 l1 (length l1)
                                in
                                  forLoop1 (j+1) j_stop lis
                                end
                            )
              )
           end
     in
        forLoop1 0 (k-count) l
     end

(* We check if it is possible to create what is asked
* and then call the algorithm
* and in the end we print the solution *)
fun solve (number,k) =
    let
        fun solution (number, k) =
           let
                val (l,count) = decimal_to_binary(number)
           in
                (
                        if ( (k>number) orelse (k<count) ) then nil
                        else (
                                algorithm (reverse l) k count
                        )
                )
           end
    in
      let
        val l = solution(number,k)
      in
        (print("["); print_list l (length l))
      end
    end

(* This function runs for all the (10) n,k that were asked *)
fun solveAll (n, nil) = ()
 |  solveAll (n, number::k::restlist) = (solve (number, k); solveAll (n,restlist))
 |  solveAll (n, number::restlist) = print("sth wrrong\n")


fun handleFile fileName = solveAll (parse fileName)

(* Uncomment the following lines ONLY for MLton submissions. *)
val _ =  handleFile (hd (CommandLine.arguments()))

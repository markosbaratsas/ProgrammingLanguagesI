(* for SML/NJ *)
(* A function to read an integer from specified input. *)
fun readInt input =
   Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)


(* Define some global arrays that are too big...
*  We are going to use the part of them that is needed for each graph. *)
(* We use the same names of variables as the c++ file. *)
val max = 1000000;
val color = Array.array(max, 0); 
val par = Array.array(max, 0); 
val mark = Array.array(max, 0); 
(* graph vector is defined from parse file... *)
 (* The i'th element of the array is a list containing the 
  * neighbors of the i'th element.
  * We initiate all elements of the array in ([0]).
  * In the 0'th element we save ni, mi in the list. 
  * hd l is ni, tl l is mi. *)
val cycles = Array.array(max, [0]); 
val graph = Array.array(max, [0]);
(* We use cycleNumer as a variable and we can change its value! *)
val cycleNumber = Array.array(1, 0);
(* We use it to count the nodes of each sub-tree. *)
val countNodes = Array.array(1, 0);
val countNumberNodes = Array.array(max, 0);

fun parseGraph inStream i =
    let
        (* Read an integer (number of countries) and consume newline. *)
        val ni = readInt inStream
        val mi = readInt inStream 

        fun clearArray givenArray 0 = (Array.update(givenArray,0,0))
          | clearArray givenArray i =
               (Array.update(givenArray,i,0);
               clearArray givenArray (i-1)) 
      
        fun clearArrayOfLists givenArray 0 = (Array.update(givenArray,0,(ni::[mi])))
          | clearArrayOfLists givenArray i = 
               (Array.update(givenArray,i,([0]));
               clearArrayOfLists givenArray (i-1))

        (* Here we "read" the graph: we fix the names of the nodes and create
        *  we set the neighboors of each node. *)
        fun createListOfArrays 0 arr = arr 
          | createListOfArrays i arr = 
                                  let
                                    val node1 = readInt inStream
                                    val node2 = readInt inStream 
                                    val neighboors1 = Array.sub(arr, node1)
                                    val neighboors2 = Array.sub(arr, node2)
                                  in
                                    ((if(hd neighboors1 = 0) then
                                      (Array.update(arr,node1,([node2])))
                                     else(Array.update(arr,node1,(node2::neighboors1))));
                                    (if(hd neighboors2 = 0) then
                                      (Array.update(arr,node2,([node1])))
                                     else(Array.update(arr,node2,(node1::neighboors2))));
                                     createListOfArrays (i - 1) arr)
                                  end

    (* Creating a list of arrays. Each array is a graph *)
    in
         (if(i <= 0) then graph 
          else (Array.update(cycleNumber,0,0);
                clearArrayOfLists graph (ni+1);
                clearArrayOfLists cycles (ni+1);
                clearArray mark (ni+1);
                clearArray color (ni+1);
                clearArray par (ni+1);
                clearArray countNumberNodes (ni+1);
                (createListOfArrays mi graph)))
    end

(* Finding the number of graphs that are going to be checked. *)
fun parseNumberOfGraphs inStream = (readInt inStream )

(* Prints the list in the way we need it. *)
fun print_list l length =
        if((hd l) = 0) then print_list (tl l) (length-1)
        else (
        if(length = 0) then print("\n") else (
                if((length = 1)) then print(Int.toString(hd l) ^ "\n")
                else (print(Int.toString(hd l) ^ " "); print_list (tl l)
                (length-1))))


(* Function to mark the vertex with different colors for different cycles.
*  (Coronographs need one cycle, but this works for more than one cycle as well)
*  It's working like the corresponding function in the c++ file. *)
fun dfs_cycle u p = 
        let
          fun handlingColorOne newCyclenumber = 
                let
                  val cur = p
                  fun whileLoop cur u = 
                      if(cur = u) then ()
                      else (
                        let
                          val newCur = Array.sub(par,cur)
                        in 
                          (Array.update(mark,newCur,newCyclenumber);
                           whileLoop newCur u)
                        end 
                    )
                in
                  (Array.update(mark,cur,newCyclenumber);
                   whileLoop cur u;
                   newCyclenumber
                  )
                end
        in
          (
          if(Array.sub(cycleNumber,0) > 1) then Array.sub(cycleNumber,0) 
          else(
          if(Array.sub(color,u) = 2) then (
          Array.sub(cycleNumber,0))
          else (if (Array.sub(color,u) = 1) then (
                        (* cyclenumber++ *)
                        (let 
                          val currentCycleNumber = Array.sub(cycleNumber,0)
                        in 
                          Array.update(cycleNumber,0,(currentCycleNumber+1))
                        end);
                        (handlingColorOne (Array.sub(cycleNumber,0))
                        )
                )
                else (
                        let
                          val graphUlist = Array.sub(graph,u) 
                          fun forLoop nil u = ()
                            | forLoop l u = (
                                let 
                                  val v = (hd l)
                                  val parU = Array.sub(par,u)
                                in
                                  (
                                  if(v = parU) then (forLoop (tl l) u)
                                   else (dfs_cycle v u; (forLoop (tl l) u)))
                                end 
                           )
                        in
                          (
                          (Array.update(par,u,p));
                          (Array.update(color,u,1));
                           (forLoop graphUlist u);
                           (Array.update(color,u,2));
                           Array.sub(cycleNumber,0)
                          )
                        end

                )
          )
          )
          )
        end


(* Find the number of nodes of the sub-tree of coronograph. *)
fun findNumberNodes u =
        let
          fun findLoop2 nil nodeWeSee = true
            | findLoop2 cyclesXList nodeWeSee =
                     if((hd cyclesXList) = nodeWeSee) then false
                     else (findLoop2 (tl cyclesXList) nodeWeSee)
        in
          (
          if (Array.sub(color,u) = 2) then (
                        let
                          val graphUlist = Array.sub(graph,u) 
                          fun forLoop nil u = ()
                            | forLoop (v::restList) u = (
                                if(Array.sub(mark,v) = 1) then (forLoop restList u)
                                else ((findNumberNodes v); (forLoop restList u))
                            )
                        in
                          (
                           (let
                             val curCountNodes = Array.sub(countNodes,0)
                           in
                             (Array.update(countNodes,0,(curCountNodes+1)))
                           end);
                           (Array.update(color,u,3));
                           (forLoop graphUlist u);
                           Array.sub(countNodes,0)
                          )
                        end
          )
          else (
          Array.sub(countNodes,0))
          )
        end


(* Handle the printing of the graph. *)
fun printingGraph mi = 
        let 
          fun forLoop1 i edges =
                        if(i > edges) then ()
                        else (
                        let
                          val markI = Array.sub(mark,i) 
                          val nodesCyclesSoFar = Array.sub(cycles,markI)
                        in (
                        if(markI <> 0) then (if (hd nodesCyclesSoFar = 0) 
                                             then (Array.update(cycles,markI,[i]); (forLoop1 (i+1) edges))
                                             else
                                               (Array.update(cycles,markI,i::nodesCyclesSoFar); (forLoop1 (i+1) edges)))
                        else (
                                (forLoop1 (i+1) edges)
                        )
                        )
                        end
                        )

          fun forLoop2 nil i = ()
            | forLoop2 cycleList  i = 
                        let
                          val x = hd cycleList
                          fun forLoop3 nil = (Array.sub(countNodes,0))
                            | forLoop3 (graphXI::restList) = 
                                let
                                  val markGraphXI = Array.sub(mark,graphXI) 
                                in
                                  if(markGraphXI = 1)
                                  then (forLoop3 restList) 
                                  else ((findNumberNodes graphXI); forLoop3 restList)
                                end
                          val currentCountNumberNodes = Array.sub(countNumberNodes,0)
                          val newI = (i+1)
                        in
                          (
                          (Array.update(countNodes,0,1));
                          forLoop3 (Array.sub(graph,x));
                          (Array.update(countNumberNodes,i,(Array.sub(countNodes,0))));
                          (forLoop2 (tl cycleList) newI) 
                          )
                        end
        in
          (
          (forLoop1 1 mi);
          (
          let 
            val coronaSize = length (Array.sub(cycles,1))
          in
            print("CORONA " ^ Int.toString(coronaSize) ^ "\n")
          end
           );
           (forLoop2 (Array.sub(cycles,1)) 0)
          )
        end


(* It's like the int main() of the c++ file, 
 * but the initialization to 0 of the arrays 
 * has already been done in fun parsefile. *)
fun solve graph = 
        let
          val ni = (hd (Array.sub(graph,0)))
          val mi = (hd (tl (Array.sub(graph,0))))
          fun isCoherent color 0 = true
            | isCoherent color i = 
             if(Array.sub(color,i) = 0) then false
                                        else (isCoherent color (i-1)) 
        in
         (if(ni=mi) then (
              let
                val cyclenumber = (dfs_cycle 1 0)
              in 
                if(isCoherent color ni) then (
                (* Doing the printing thing(by calling an external function).*)
                        if(cyclenumber = 1) then ( 
                        let
                          val doTheThing = (printingGraph mi)
                          fun halve nil = (nil, nil)
                            | halve [a] = ([a], nil)
                            | halve (a::b::cs) =
                                let
                                   val (x, y) = halve cs
                                in
                                   (a::x, b::y)
                                end; 
                           fun merge (nil, ys) = ys
                             | merge (xs, nil) = xs
                             | merge (x::xs, y::ys) =
                                if x < y then x :: merge (xs, y::ys)
                                else y :: merge (x::xs, ys);
                          fun mergeSort nil = nil
                            | mergeSort [a] = [a]
                            | mergeSort theList =
                                let
                                   val (x, y) = halve theList
                                in
                                   merge (mergeSort x, mergeSort y)
                                end;
                          fun convertToList ~1 l = l
                            | convertToList i l = 
                                let 
                                  val x = Array.sub(countNumberNodes,i)
                                  val newI = (i-1)
                                in 
                                  (convertToList newI (x::l))
                                end
                          val almostFinalList = (convertToList (length (Array.sub(cycles,1))) [])
                          val finalList = (mergeSort almostFinalList)
                        in
                          (print_list finalList (length finalList))
                        end
                        )
                        else print("NO CORONA\n")
                )
                else print("NO CORONA\n")
              end
          )
          else print("NO CORONA\n")
          )
       end
           


(* This function runs for all the (10) n,k that were asked *)
fun solveAll graph 1 fileName = (solve graph)
 |  solveAll graph n fileName = ((solve graph); (solveAll (parseGraph fileName (n-1)) (n-1) fileName))

fun coronograph fileName = 
  let
    val inStream = TextIO.openIn fileName
    val n = parseNumberOfGraphs inStream
    val graph = parseGraph inStream n
  in 
    (solveAll graph n inStream)
  end

(* Uncomment the following lines ONLY for MLton submissions. *)
val _ =  coronograph (hd (CommandLine.arguments())) 

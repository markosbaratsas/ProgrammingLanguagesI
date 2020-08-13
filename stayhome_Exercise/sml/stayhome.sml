(* MLton submission *)
(* for SML/NJ submission see in the very end of this file *)


(* Define some global arrays that are too big...
*  We are going to use the part of them that is needed for each graph. *)
(* We use the same names of variables as the Python file. *)
(* We define all arrays in max size and we use only what we need from them
 * using global parameters row,col *)
val max = 1001;

(* All variables are initialized to sth (almost) random
 * and later in the program they wiil take their real values *)
val mat = Array2.array(max,max,".");

val row = Array.array(1,0);
val col = Array.array(1,0);
val s = Array.array(2,~1);
val t = Array.array(2,~1);
val w = Array.array(2,~1);

val airports_counter = Array.array(1,0);
val airports = Array.array(max*max,(~1)::[~1]);
val visitedPerson = Array2.array(max,max,false);
val visitedCorona = Array2.array(max,max,false);
val qPerson = Array.array(max*max,[(~1)::(~1)::(~1)::[]]);
val qCorona = Array.array(max*max,[(~1)::(~1)::(~1)::[]]);

val previousRow = Array2.array(max,max,~1);
val previousCol = Array2.array(max,max,~1);

(* Passes the char list to the global array in the way we need it. *)
(* Tracks S, T, W and all airports *)
fun charList_pass_to_array nil sth sthElse = ()
 |  charList_pass_to_array l lines_counter j =
        if((hd l) = #"\n") then ()
        else (
                (if((hd l) = #"S") then (
                                        Array.update(s,0,lines_counter);
                                        Array.update(s,1,j)) else());
                (if((hd l) = #"T") then (
                                        Array.update(t,0,lines_counter);
                                        Array.update(t,1,j)) else());
                (if((hd l) = #"W") then (
                                        Array.update(w,0,lines_counter);
                                        Array.update(w,1,j)) else());
                (if((hd l) = #"A") then (
                                        (let
                                          val previous = Array.sub(airports_counter, 0)
                                          val current = (previous+1)
                                        in
                                          Array.update(airports_counter,0,current);
                                          Array.update(airports,previous,(lines_counter::[j]))
                                        end)) else());
                Array2.update(mat,lines_counter,j, Char.toString(hd l) );
                charList_pass_to_array (tl l) lines_counter (j+1))

(* Prints the string list in the way we need it. *)
fun print_string_list nil = ()
 |  print_string_list l =
        if((length l) = 1) then print((hd l) ^ "\n")
        else (print(hd l); print_string_list (tl l))

(* Prints the array the way we need it. *)
(* Used for debugging *)
fun print_string_Array2 0 j = ()
 |  print_string_Array2 i j =
        let
          val printingI = ((Array.sub(row,0)) - i)
          fun print_row i 0 = ()
            | print_row i j = (
                        let
                          val printingJ = ((Array.sub(col,0)) - j)
                        in
                          (
                           print("  |  ");
                           print(Array2.sub(mat,printingI,printingJ));
                           print_row i (j-1)
                           )
                        end)
        in
          (
           print_row i j;
           print("  |\n");
           print_string_Array2 (i-1) j
          )
        end

(* Function that reads the file filling the global variables with their real values *)
fun parse file =
    let
        fun readingAllLines(inPut : string) =
                let
                   val inStream = TextIO.openIn inPut
                   val readLine = TextIO.inputLine inStream
                   fun helper(readLine : string option) lines_counter =
                       case readLine of
                          NONE => (
                                    Array.update(row,0,lines_counter);
                                    TextIO.closeIn inStream
                                  )
                        | SOME(c) => ((let
                                           val cList = explode c
                                       in
                                          (
                                           Array.update(col,0,((length cList) -1));
                                           charList_pass_to_array cList lines_counter 0
                                          )
                                       end);
                                       helper(TextIO.inputLine inStream) (lines_counter+1))
                in
                   helper readLine 0
                end

    (* Creating a list of arrays. Each array is a graph *)
    in
        readingAllLines(file)
    end


(* Most of the arrays are initialized as global objects
 * because they are of size max*max = 1000000 *)
(* It uses the same algorithm as this in the Python file*)
fun BFSstayhome () =
    let
      val personX = Array.sub(s,0)
      val personY = Array.sub(s,1)
      val coronaX = Array.sub(w,0)
      val coronaY = Array.sub(w,1)
      val destX = Array.sub(t,0)
      val destY = Array.sub(t,1)
      val queueNode0 = (personX::personY::0::[])
      val queueNode1 = (coronaX::coronaY::0::[])
      val visitedAirports = Array.array(1,false)

      val rowNum = Array.array(4,0)
      val fixRowArray = (Array.update(rowNum,0,1); Array.update(rowNum,1,0);
                        Array.update(rowNum,2,0); Array.update(rowNum,3,(~1)))
      val colNum = Array.array(4,0)
      val fixColNum = (Array.update(colNum,0,0); Array.update(colNum,1,(~1));
                        Array.update(colNum,2,1); Array.update(colNum,3,0))

      fun isValid rowN colN = ((rowN >= 0) andalso (rowN < (Array.sub(row,0))) andalso
                               (colN >= 0) andalso (colN < (Array.sub(col,0))))

      (* fun whileLoopBFS timeCount "~1" "~1" symbolizes that we havent find the solution yet...
       * If we find the solution it will return it from:    | whileLoopBFS timeCount answer *)
      fun whileLoopBFS timeCount "~1" = (
        let
          val qPersonTimeCount = Array.sub(qPerson,timeCount) (* list of "queueNodes"*)
          val qCoronaTimeCount = Array.sub(qCorona,timeCount) (* list of "queueNodes"*)
          val coronaReachedDest = Array.array(1,false)
        in
          if( qPersonTimeCount = [] ) then "IMPOSSIBLE" else (
             (* ---- code for corona *)
             (let
                fun forLoopCorona qCoronaTimeCount =
                  if((hd qCoronaTimeCount) = ((~1)::(~1)::(~1)::[])) then ()
                  else (
                      let
                        (* I wanted to do   val (ptCoronaX::ptCoronaY::ptCoronaDist) = (hd qCoronaTimeCount) *)
                        (* But I got warnings while compiling, but these warning will not lead to an error...
                         * So this way I overcame warnings... *)
                        val someList = (hd qCoronaTimeCount)
                        val ptCoronaX = (hd someList)
                        val rstList = (tl someList)
                        val ptCoronaY = (hd rstList)
                        val lstlist = (tl someList)
                        val ptCoronaDist = (hd lstlist)
                      in (
                        Array2.update(mat,ptCoronaX,ptCoronaY,"W");
                        if((ptCoronaX = (Array.sub(t,0))) andalso (ptCoronaY = (Array.sub(t,1))))
                        then (
                                Array.update(coronaReachedDest,0,true)
                        )
                        else (
                                let
                                  fun insideLoopCorona 4 = ()
                                    | insideLoopCorona i = (
                                        let
                                          val currentRow = (ptCoronaX + (Array.sub(rowNum,i)))
                                          val currentCol = (ptCoronaY + (Array.sub(colNum,i)))
                                        in
                                          if((isValid currentRow currentCol) andalso
                                             ((Array2.sub(mat,currentRow,currentCol) = "s") orelse
                                             (Array2.sub(mat,currentRow,currentCol) = ".") orelse
                                             (Array2.sub(mat,currentRow,currentCol) = "S") orelse
                                             (Array2.sub(mat,currentRow,currentCol) = "T")) andalso
                                             (not (Array2.sub(visitedCorona,currentRow,currentCol))))
                                           then (
                                                Array2.update(visitedCorona,currentRow,currentCol,true);
                                                (let
                                                  val adjcell =(currentRow::currentCol::(timeCount+2)::[])
                                                  val previousqCorona = Array.sub(qCorona,(timeCount+2))
                                                in
                                                  Array.update(qCorona,(timeCount+2),(adjcell::previousqCorona))
                                                end);
                                                insideLoopCorona (i+1)
                                          ) else (
                                                if((isValid currentRow currentCol) andalso
                                                   ((Array2.sub(mat,currentRow,currentCol) = "A") orelse
                                                   (Array2.sub(mat,currentRow,currentCol) = "sA")) andalso
                                                   (not (Array.sub(visitedAirports,0))) andalso
                                                   (not (Array2.sub(visitedCorona,currentRow,currentCol))))
                                                 then (
                                                      Array.update(visitedAirports,0,true);
                                                      Array2.update(visitedCorona,currentRow,currentCol,true);
                                                      (let
                                                          val adjcell =(currentRow::currentCol::(timeCount+2)::[])
                                                          val previousqCorona = Array.sub(qCorona,(timeCount+2))
                                                       in
                                                           Array.update(qCorona,(timeCount+2),(adjcell::previousqCorona))
                                                       end);
                                                       (let
                                                         fun airportForLoop (~1) = ()
                                                           | airportForLoop j =
                                                                let
                                                                  (* Like in qCoronaTimeCount, I wanted to do   val (airportsJx::airportsJy::rst) = Array.sub(airports,j) *)
                                                                  (* But I got warnings while compiling, but these warning will not lead to an error...
                                                                   * So this way I overcame warnings... *)
                                                                  val someListA = Array.sub(airports,j)
                                                                  val airportsJx = (hd someListA)
                                                                  val rstListA = (tl someListA)
                                                                  val airportsJy = (hd rstListA)
                                                                in
                                                                  (Array2.update(visitedCorona,airportsJx,airportsJy,true);
                                                                  (let
                                                                      val adjcell =(airportsJx::airportsJy::(timeCount+7)::[])
                                                                      val previousqCorona = Array.sub(qCorona,(timeCount+7))
                                                                  in
                                                                      Array.update(qCorona,(timeCount+7),(adjcell::previousqCorona))
                                                                   end);
                                                                   airportForLoop (j-1))
                                                                end
                                                       in
                                                         airportForLoop (Array.sub(airports_counter,0) -1)
                                                       end);
                                                       insideLoopCorona (i+1)
                                                   )
                                                else (insideLoopCorona (i+1)))
                                        end)
                                in
                                   insideLoopCorona 0;
                                   forLoopCorona (tl qCoronaTimeCount)
                                end
                        )
                      )
                      end
                  )

                  (* ---- code for person *)
                  fun forLoopPerson qPersonTimeCount =
                    if( qPersonTimeCount = [] ) then ("~1" )
                    else (
                          let
                            (* Like in qPersonTimeCount, I wanted to do   val ptPersonX::ptPersonY::ptPersonDist = hd qPersonTimeCount *)
                            (* But I got warnings while compiling, but these warning will not lead to an error...
                             * So this way I overcame warnings... *)
                            val someList = (hd qPersonTimeCount)
                            val ptPersonX = (hd someList)
                            val rstList = (tl someList)
                            val ptPersonY = (hd rstList)
                            val lstlist = (tl someList)
                            val ptPersonDist = (hd lstlist)
                          in (
                            if((ptPersonX = (Array.sub(t,0))) andalso (ptPersonY = (Array.sub(t,1))))
                            then (
                                    (Int.toString (timeCount))
                            )
                            else (
                              if((Array2.sub(mat,ptPersonX,ptPersonY) = "W"))
                              then (
                                      forLoopPerson (tl qPersonTimeCount)
                              )
                              else(
                                if(Array2.sub(mat,ptPersonX,ptPersonY) = "A") then ( Array2.update(mat,ptPersonX,ptPersonY,"sA") )
                                                                              else ( Array2.update(mat,ptPersonX,ptPersonY,"s") );
                                (let
                                  fun insideLoopPerson 4 = ()
                                    | insideLoopPerson i = (
                                        let
                                          val currentRow = (ptPersonX + (Array.sub(rowNum,i)))
                                          val currentCol = (ptPersonY + (Array.sub(colNum,i)))
                                        in
                                          if((isValid currentRow currentCol) andalso
                                             ((Array2.sub(mat,currentRow,currentCol) = ".") orelse
                                             (Array2.sub(mat,currentRow,currentCol) = "A") orelse
                                             (Array2.sub(mat,currentRow,currentCol) = "T")) andalso
                                             (not (Array2.sub(visitedPerson,currentRow,currentCol))))
                                           then (
                                                Array2.update(visitedPerson,currentRow,currentCol,true);
                                                Array2.update(previousRow,currentRow,currentCol,ptPersonX);
                                                Array2.update(previousCol,currentRow,currentCol,ptPersonY);
                                                (let
                                                  val adjcell =(currentRow::currentCol::(timeCount+1)::[])
                                                  val previousqPerson = Array.sub(qPerson,(timeCount+1))
                                                in
                                                  (* list1@list2 may not seem that efficient, but it is *
                                                   * because **here** list1 and list2 will be lists of 2-3 elements max.. *)
                                                  Array.update(qPerson,(timeCount+1),(previousqPerson@[adjcell]))
                                                end)
                                          ) else ();
                                          insideLoopPerson (i+1)
                                        end)
                                in
                                   insideLoopPerson 0;
                                   forLoopPerson (tl qPersonTimeCount)
                                end
                                )
                              )
                            )
                          )
                          end
                      )
              in(
                forLoopCorona qCoronaTimeCount;
                Array.update(qPerson,(timeCount+1),[]);
                if(not (Array.sub(coronaReachedDest,0))) then( whileLoopBFS (timeCount+1) (forLoopPerson qPersonTimeCount))
                else( whileLoopBFS (timeCount+1) ("IMPOSSIBLE") )
                )
              end)
        )
        end)
        | whileLoopBFS timeCount answer = (answer)  (* return that sth: sth is "IMPOSSIBLE" || timeCount *)
    in
      (
        Array2.update(visitedPerson,personX,personY,true);
        (let
          val qPersonPrevious = Array.sub(qPerson,0)
          val qCoronaPrevious = Array.sub(qCorona,0)
         in
          Array.update(qPerson,0,[queueNode0]);
          Array.update(qCorona,0,queueNode1::qCoronaPrevious)
         end);
        whileLoopBFS 0 "~1"
     )
    end

(* We go from the destination node(T), towards the first node(S),
 * by finding it's cell's previous cell.
 * It uses the same algorithm as this in the Python file  *)
fun findPath countNumber =
  let
    val pointRow = Array.sub(t,0)
    val pointCol = Array.sub(t,1)
    val srcX = Array.sub(s,0)
    val srcY = Array.sub(s,1)
    fun whileLoop countNumber (~1) (~1) srcX srcY l = (["~1"], countNumber)
      | whileLoop countNumber currentPointRow currentPointCol srcX srcY l = (
          if((currentPointRow = srcX) andalso (currentPointCol = srcY)) then (l,countNumber) else (
            let
              val newPointRow = Array2.sub(previousRow,currentPointRow,currentPointCol)
              val newPointCol = Array2.sub(previousCol,currentPointRow,currentPointCol)
            in
              (* Keep in mind that we are going backwards(in the opposite direction...) *)
              if(newPointRow > currentPointRow) then (whileLoop (countNumber+1) newPointRow newPointCol srcX srcY ("U"::l)) else (
              if(newPointRow < currentPointRow) then (whileLoop (countNumber+1) newPointRow newPointCol srcX srcY ("D"::l)) else (
              if(newPointCol < currentPointCol) then (whileLoop (countNumber+1) newPointRow newPointCol srcX srcY ("R"::l)) else (
              if(newPointCol > currentPointCol) then (whileLoop (countNumber+1) newPointRow newPointCol srcX srcY ("L"::l)) else (
              whileLoop (countNumber+1) newPointRow newPointCol srcX srcY ("SthWrong"::l)))))
            end))
  in
    (whileLoop countNumber pointRow pointCol srcX srcY [])
  end


(* It's like the def main() of the Python file *)
fun stayhome file =
  let
    val read_file_fix_mat = parse file
    val personDist = BFSstayhome ()
  in
    if(personDist = "IMPOSSIBLE") then (print("IMPOSSIBLE\n"))
    else (
      print(personDist^"\n");
      (let
        val (returningList, countNumber) = findPath 0
      in
        print_string_list (returningList)
      end )
      )
  end

(* Uncomment the following lines ONLY for MLton submissions. *)
val _ =  stayhome (hd (CommandLine.arguments()))

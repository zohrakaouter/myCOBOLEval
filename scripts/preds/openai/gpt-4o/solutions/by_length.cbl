       IDENTIFICATION DIVISION.
       PROGRAM-ID. BY-LENGTH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ARRAY.
           05 WS-ARR OCCURS 8 TIMES INDEXED BY W-S-INDEX PIC X(2).
       
       01 WS-RESULT-ITEMS.
           05 WS-NAME OCCURS 9 TIMES PIC X(10)
                  VALUE 'One     ' 'Two     ' 'Three   ' 'Four    '
                        'Five    ' 'Six     ' 'Seven   ' 'Eight   '
                        'Nine    '.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 8 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC X(100).

      * 
      * Given an array of integers, sort the integers that are between 1 and 9 inclusive,
      * reverse the resulting array, and then replace each digit by its corresponding name from
      * "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine".
      * 
      * For example:
      * arr = [2, 1, 1, 4, 5, 8, 2, 3]
      * -> sort arr -> [1, 1, 2, 2, 3, 4, 5, 8]
      * -> reverse arr -> [8, 5, 4, 3, 2, 2, 1, 1]
      * return ["Eight", "Five", "Four", "Three", "Two", "Two", "One", "One"]
      * 
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN-PROGRAM.
           PERFORM INITIALIZE-ARRAY
           PERFORM SORT-ARRAY
           PERFORM REVERSE-ARRAY
           PERFORM MAP-NUMBERS-TO-NAMES
           GOBACK
           .
           
       INITIALIZE-ARRAY.
           MOVE L-ARR(1) TO WS-ARR(1)
           MOVE L-ARR(2) TO WS-ARR(2)
           MOVE L-ARR(3) TO WS-ARR(3)
           MOVE L-ARR(4) TO WS-ARR(4)
           MOVE L-ARR(5) TO WS-ARR(5)
           MOVE L-ARR(6) TO WS-ARR(6)
           MOVE L-ARR(7) TO WS-ARR(7)
           MOVE L-ARR(8) TO WS-ARR(8)
           .
           
       SORT-ARRAY.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 8
               PERFORM VARYING NJ FROM NI BY 1 UNTIL NJ > 8
                   IF WS-ARR(NI) > WS-ARR(NJ)
                       MOVE WS-ARR(NJ) TO RESULT(1)
                       MOVE WS-ARR(NI) TO WS-ARR(NJ)
                       MOVE RESULT(1) TO WS-ARR(NI)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       
       REVERSE-ARRAY.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 8
               MOVE WS-ARR(NI) TO RESULT(9 - NI)
           END-PERFORM
           .
           
       MAP-NUMBERS-TO-NAMES.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 8
               MOVE FUNCTION NUMVAL(RESULT(NI)) TO W-S-INDEX
               MOVE WS-NAME(W-S-INDEX) TO RESULT(NI)
           END-PERFORM
           .
           
       END PROGRAM BY-LENGTH.

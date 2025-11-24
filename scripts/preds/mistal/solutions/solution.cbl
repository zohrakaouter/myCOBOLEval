       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLUTION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC 9(2) VALUE 0.
       01 WS-TEMP-SUM PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Given a non-empty list of integers, return the sum of all of the odd elements that are in even positions.
      * 
      * 
      * Examples
      * solution([5, 8, 7, 1]) ==> 12
      * solution([3, 3, 3, 3, 3]) ==> 9
      * solution([30, 13, 24, 321]) ==>0
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 4
               IF WS-COUNTER IS EVEN
                   IF FUNCTION MOD(L-LST(WS-COUNTER), 2) NOT EQUAL TO 0
                       ADD L-LST(WS-COUNTER) TO WS-TEMP-SUM
                   END-IF
               END-IF
           END-PERFORM
           MOVE WS-TEMP-SUM TO RESULT
           GOBACK.
       END PROGRAM SOLUTION.

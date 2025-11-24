       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAN-ARRANGE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-INDEX           PIC S9(10) COMP-3.
       01 WS-PREVIOUS-ELEM   PIC S9(10) COMP-3.
       01 WS-CURRENT-ELEM    PIC S9(10) COMP-3.
       01 WS-FOUND           PIC S9(10) COMP-3 VALUE -1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Create a function which returns the largest index of an element which
      * is not greater than or equal to the element immediately preceding it. If
      * no such element exists then return -1. The given array will not contain
      * duplicate values.
      * 
      * Examples:
      * can_arrange([1,2,4,3,5]) = 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 2 BY 1 UNTIL NI > 5
               SET WS-PREVIOUS-ELEM TO L-ARR(NI - 1)
               SET WS-CURRENT-ELEM TO L-ARR(NI)
               
               IF WS-CURRENT-ELEM < WS-PREVIOUS-ELEM THEN
                   SET WS-FOUND TO NI
               END-IF
           
           END-PERFORM

           IF WS-FOUND > 0 THEN
               SUBTRACT 1 FROM WS-FOUND
           END-IF

           IF WS-FOUND = 0 THEN
               ADD 1 TO WS-FOUND
               SUBTRACT 1 FROM WS-FOUND
           END-IF

           MOVE WS-FOUND TO RESULT

           GOBACK.

       END PROGRAM CAN-ARRANGE.

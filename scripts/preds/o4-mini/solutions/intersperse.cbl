       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTERSPERSE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 L-DELIMETER PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Insert a number 'delimeter' between every two consecutive elements of input list `numbers'
      * >>> intersperse([2, 2, 2], 4)
      * [2, 4, 2, 4, 2]
      * >>> intersperse([1, 2, 3], 4)
      * [1, 4, 2, 4, 3]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               MOVE L-NUMBERS(NI) TO RESULT(NJ)
               ADD 1 TO NJ
               IF NI < 3
                   MOVE L-DELIMETER TO RESULT(NJ)
                   ADD 1 TO NJ
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM INTERSPERSE.

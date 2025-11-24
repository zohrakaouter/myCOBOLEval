       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROLLING-MAX.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-MAX-VALUE PIC S9(10) VALUE 0.
       01 WS-IDX      PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMBERS OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * From a given list of integers, generate a list of rolling maximum element found until given moment
      * in the sequence.
      * >>> rolling_max([1, 2, 3, 2])
      * [1, 2, 3, 3]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 4
               IF NI = 1
                   MOVE L-NUMBERS(NI) TO WS-MAX-VALUE
                   MOVE WS-MAX-VALUE TO RESULT(NI)
               ELSE
                   IF L-NUMBERS(NI) > WS-MAX-VALUE
                       MOVE L-NUMBERS(NI) TO WS-MAX-VALUE
                   END-IF
                   MOVE WS-MAX-VALUE TO RESULT(NI)
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM ROLLING-MAX.

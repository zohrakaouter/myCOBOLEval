       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGEST-DIVISOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * For a given number n, find the largest number that divides n evenly, smaller than n
      * >>> largest_divisor(15)
      * 5
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-N TO RESULT
           PERFORM VARYING WS-COUNTER FROM 2 BY 1 UNTIL WS-COUNTER >= L-N
               IF FUNCTION MOD(L-N, WS-COUNTER) = 0 THEN
                   MOVE WS-COUNTER TO RESULT
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM LARGEST-DIVISOR.

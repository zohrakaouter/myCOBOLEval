       IDENTIFICATION DIVISION.
       PROGRAM-ID. STARTS-ONE-ENDS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-COUNT PIC S9(10).
       01 WS-FACTOR PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Given a positive integer n, return the count of the numbers of n-digit
      * positive integers that start or end with 1.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           
           IF L-N <= 0 
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           COMPUTE WS-FACTOR = 10 ** (L-N - 1)

           * Calculate numbers starting with 1
           COMPUTE WS-COUNT = WS-FACTOR

           * Calculate numbers ending with 1
           COMPUTE RESULT = WS-COUNT + WS-FACTOR

           * If n is greater than 1, adjust for double-counted "11" numbers 
           IF L-N > 1 
               SUBTRACT WS-FACTOR FROM RESULT
           END-IF

           GOBACK.

       END PROGRAM STARTS-ONE-ENDS.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIB4.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01  WS-COUNTER          PIC S9(10).
       01  WS-TEMP1            PIC S9(10).
       01  WS-TEMP2            PIC S9(10).
       01  WS-TEMP3            PIC S9(10).
       01  WS-TEMP4            PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * The Fib4 number sequence is a sequence similar to the Fibbonacci sequnece that's defined as follows:
      * fib4(0) -> 0
      * fib4(1) -> 0
      * fib4(2) -> 2
      * fib4(3) -> 0
      * fib4(n) -> fib4(n-1) + fib4(n-2) + fib4(n-3) + fib4(n-4).
      * Please write a function to efficiently compute the n-th element of the fib4 number sequence.  Do not use recursion.
      * >>> fib4(5)
      * 4
      * >>> fib4(6)
      * 8
      * >>> fib4(7)
      * 14
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           IF L-N = 0 OR L-N = 1 OR L-N = 3
               MOVE 0 TO RESULT
           ELSE IF L-N = 2
               MOVE 2 TO RESULT
           ELSE
               MOVE 0 TO WS-TEMP1
               MOVE 0 TO WS-TEMP2
               MOVE 2 TO WS-TEMP3
               MOVE 0 TO WS-TEMP4
               MOVE 0 TO RESULT

               PERFORM VARYING WS-COUNTER FROM 4 BY 1 UNTIL WS-COUNTER > L-N
                   COMPUTE RESULT = WS-TEMP1 + WS-TEMP2 + WS-TEMP3 + WS-TEMP4
                   MOVE WS-TEMP3 TO WS-TEMP1
                   MOVE WS-TEMP4 TO WS-TEMP2
                   MOVE RESULT TO WS-TEMP3
                   MOVE WS-TEMP2 TO WS-TEMP4
               END-PERFORM
           END-IF.

           GOBACK.
       END PROGRAM FIB4.

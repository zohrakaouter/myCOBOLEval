       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBFIB.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC S9(10).
       01 WS-TEMP1 PIC S9(10) VALUE 0.
       01 WS-TEMP2 PIC S9(10) VALUE 0.
       01 WS-TEMP3 PIC S9(10) VALUE 1.
       01 WS-NEXT PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * The FibFib number sequence is a sequence similar to the Fibbonacci sequnece that's defined as follows:
      * fibfib(0) == 0
      * fibfib(1) == 0
      * fibfib(2) == 1
      * fibfib(n) == fibfib(n-1) + fibfib(n-2) + fibfib(n-3).
      * Please write a function to efficiently compute the n-th element of the fibfib number sequence.
      * >>> fibfib(1)
      * 0
      * >>> fibfib(5)
      * 4
      * >>> fibfib(8)
      * 24
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           IF L-N = 0 OR L-N = 1
               MOVE 0 TO RESULT
           ELSE IF L-N = 2
               MOVE 1 TO RESULT
           ELSE
               MOVE 2 TO WS-COUNTER
               PERFORM UNTIL WS-COUNTER >= L-N
                   COMPUTE WS-NEXT = WS-TEMP1 + WS-TEMP2 + WS-TEMP3
                   MOVE WS-TEMP2 TO WS-TEMP1
                   MOVE WS-TEMP3 TO WS-TEMP2
                   MOVE WS-NEXT TO WS-TEMP3
                   ADD 1 TO WS-COUNTER
               END-PERFORM
               MOVE WS-TEMP3 TO RESULT
           END-IF.

           GOBACK.
       END PROGRAM FIBFIB.

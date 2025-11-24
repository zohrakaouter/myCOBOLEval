       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIB.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-FIB1 PIC S9(10) VALUE 0.
       01 WS-FIB2 PIC S9(10) VALUE 1.
       01 WS-TEMP PIC S9(10) VALUE 0.
       01 COUNTER PIC S9(10) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * Return n-th Fibonacci number.
      * >>> fib(10)
      * 55
      * >>> fib(1)
      * 1
      * >>> fib(8)
      * 21
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           IF L-N LESS THAN 1
               MOVE 0 TO RESULT
           ELSE
               PERFORM VARYING COUNTER FROM 2 BY 1 UNTIL COUNTER GREATER THAN L-N
                   ADD WS-FIB1 TO WS-FIB2 GIVING WS-TEMP
                   MOVE WS-FIB2 TO WS-FIB1
                   MOVE WS-TEMP TO WS-FIB2
               END-PERFORM
               MOVE WS-FIB2 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM FIB.

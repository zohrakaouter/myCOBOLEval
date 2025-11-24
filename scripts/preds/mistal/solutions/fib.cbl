       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIB.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-I PIC S9(10).
           05 WS-A PIC S9(10) VALUE 0.
           05 WS-B PIC S9(10) VALUE 1.
           05 WS-TEMP PIC S9(10).

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

           IF L-N <= 0
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           IF L-N = 1
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > L-N
               COMPUTE WS-TEMP = WS-A + WS-B
               MOVE WS-B TO WS-A
               MOVE WS-TEMP TO WS-B
           END-PERFORM

           MOVE WS-B TO RESULT

           GOBACK
           .
       END PROGRAM FIB.

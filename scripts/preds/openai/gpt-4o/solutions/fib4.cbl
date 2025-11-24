       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIB4.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-VARS.
           05 PREV3 PIC S9(10) VALUE 0.
           05 PREV2 PIC S9(10) VALUE 0.
           05 PREV1 PIC S9(10) VALUE 2.
           05 PREV0 PIC S9(10) VALUE 0.
           05 CURR  PIC S9(10).
           05 I     PIC S9(10).

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
           IF L-N LESS THAN 0 THEN
               MOVE 0 TO RESULT
           ELSE IF L-N EQUAL 0 THEN
               MOVE 0 TO RESULT
           ELSE IF L-N EQUAL 1 THEN
               MOVE 0 TO RESULT
           ELSE IF L-N EQUAL 2 THEN
               MOVE 2 TO RESULT
           ELSE IF L-N EQUAL 3 THEN
               MOVE 0 TO RESULT
           ELSE
               PERFORM VARYING I FROM 4 BY 1 UNTIL I GREATER THAN L-N
                   COMPUTE CURR = PREV0 + PREV1 + PREV2 + PREV3
                   MOVE PREV2 TO PREV3
                   MOVE PREV1 TO PREV2
                   MOVE PREV0 TO PREV1
                   MOVE CURR  TO PREV0
               END-PERFORM
               MOVE CURR TO RESULT
           END-IF
           .

       GOBACK.

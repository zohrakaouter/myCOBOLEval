       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRI.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-I PIC S9(10).
           05 WS-TEMP PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Everyone knows Fibonacci sequence, it was studied deeply by mathematicians in
      * the last couple centuries. However, what people don't know is Tribonacci sequence.
      * Tribonacci sequence is defined by the recurrence:
      * tri(1) = 3
      * tri(n) = 1 + n / 2, if n is even.
      * tri(n) =  tri(n - 1) + tri(n - 2) + tri(n + 1), if n is odd.
      * For example:
      * tri(2) = 1 + (2 / 2) = 2
      * tri(4) = 3
      * tri(3) = tri(2) + tri(1) + tri(4)
      * = 2 + 3 + 3 = 8
      * You are given a non-negative integer number n, you have to a return a list of the
      * first n + 1 numbers of the Tribonacci sequence.
      * Examples:
      * tri(3) = [1, 3, 2, 8]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 0 BY 1 UNTIL WS-I > L-N
               IF WS-I = 0
                   MOVE 1 TO RESULT(WS-I + 1)
               ELSE IF WS-I = 1
                   MOVE 3 TO RESULT(WS-I + 1)
               ELSE
                   IF FUNCTION MOD(WS-I, 2) = 0
                       COMPUTE RESULT(WS-I + 1) = 1 + (WS-I / 2)
                   ELSE
                       COMPUTE WS-TEMP = RESULT(WS-I) + RESULT(WS-I - 1)
                       COMPUTE RESULT(WS-I + 1) = WS-TEMP + RESULT(WS-I + 2)
                   END-IF
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM TRI.

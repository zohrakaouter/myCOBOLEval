       IDENTIFICATION DIVISION.
       PROGRAM-ID. F.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01  WS-COUNTERS.
           05  I PIC S9(10).
           05  J PIC S9(10).
           05  FACTORIAL-TEMP PIC S9(10) VALUE 1.
           05  SUM-TEMP PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Implement the function f that takes n as a parameter,
      * and returns a list of size n, such that the value of the element at index i is the factorial of i if i is even
      * or the sum of numbers from 1 to i otherwise.
      * i starts from 1.
      * the factorial of i is the multiplication of the numbers from 1 to i (1 * 2 * ... * i).
      * Example:
      * f(5) == [1, 2, 6, 24, 15]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L-N
               IF FUNCTION MOD(I, 2) = 0
                   COMPUTE FACTORIAL-TEMP = 1
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > I
                       COMPUTE FACTORIAL-TEMP = FACTORIAL-TEMP * J
                   END-PERFORM
                   MOVE FACTORIAL-TEMP TO RESULT(I)
               ELSE
                   COMPUTE SUM-TEMP = 0
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > I
                       COMPUTE SUM-TEMP = SUM-TEMP + J
                   END-PERFORM
                   MOVE SUM-TEMP TO RESULT(I)
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM F.

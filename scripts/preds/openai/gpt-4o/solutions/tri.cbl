       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRI.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-TRI-NUMBERS.
           05 WS-NUMBER        PIC S9(10)  VALUE 3.
           05 WS-INDEX         PIC S9(10)  VALUE 1.
           05 WS-TEMP          PIC S9(10).

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

       BEGIN-CALCULATION.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
              UNTIL WS-INDEX > L-N + 1
              
              IF WS-INDEX = 1 THEN
                 MOVE 3 TO RESULT(WS-INDEX)
              ELSE
                 IF WS-INDEX MOD 2 = 0 THEN
                    COMPUTE RESULT(WS-INDEX) = 1 + (WS-INDEX / 2)
                 ELSE
                    IF WS-INDEX = 3 THEN
                       COMPUTE RESULT(WS-INDEX) = RESULT(WS-INDEX - 1) +
                                                RESULT(WS-INDEX - 2) +
                                                RESULT(WS-INDEX + 1)
                    ELSE
                       MOVE 0 TO WS-TEMP
                       PERFORM VARYING NI FROM 1 BY 1
                         UNTIL NI > WS-INDEX - 1
                         IF RESULT(NI) > 0
                            ADD RESULT(NI) TO WS-TEMP
                         END-IF
                       END-PERFORM
                       MOVE WS-TEMP TO RESULT(WS-INDEX)
                    END-IF
                 END-IF
              END-IF
              
           END-PERFORM.

       END-CALCULATION.
       
       GOBACK.
       END PROGRAM TRI.

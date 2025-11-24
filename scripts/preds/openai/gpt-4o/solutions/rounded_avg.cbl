       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROUNDED-AVG.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-N           PIC S9(10).
       01 WS-M           PIC S9(10).
       01 WS-SUM         PIC S9(15).
       01 WS-COUNT       PIC S9(10) COMP-3.
       01 WS-AVG         PIC S9(15)V9(5) COMP-3.
       01 WS-AVG-INTEGER PIC S9(10).
       01 WS-BIN-RESULT  PIC X(100).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 L-M PIC S9(10).
           05 RESULT PIC X(100).

      * You are given two positive integers n and m, and your task is to compute the
      * average of the integers from n through m (including n and m).
      * Round the answer to the nearest integer and convert that to binary.
      * If n is greater than m, return -1.
      * Example:
      * rounded_avg(1, 5) => "0b11"
      * rounded_avg(7, 5) => -1
      * rounded_avg(10, 20) => "0b1111"
      * rounded_avg(20, 33) => "0b11010"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           MOVE L-N TO WS-N
           MOVE L-M TO WS-M

           IF WS-N > WS-M THEN
               MOVE -1 TO WS-AVG-INTEGER
               DISPLAY WS-AVG-INTEGER
           ELSE
               MOVE 0 TO WS-SUM
               MOVE 0 TO WS-COUNT

               PERFORM VARYING WS-N FROM L-N BY 1 UNTIL WS-N > L-M
                   ADD WS-N TO WS-SUM
                   ADD 1 TO WS-COUNT
               END-PERFORM

               DIVIDE WS-SUM BY WS-COUNT GIVING WS-AVG

               COMPUTE WS-AVG-INTEGER = FUNCTION INTEGER-OF-DAY (WS-AVG ROUND)

               CALL 'ITOA' USING BY VALUE WS-AVG-INTEGER
                                BY VALUE 2  * Base for conversion, 2 for binary
                                BY REFERENCE WS-BIN-RESULT

               STRING "0b" DELIMITED BY SIZE
                      WS-BIN-RESULT DELIMITED BY X"00"
                      INTO RESULT

           END-IF
           
           GOBACK.
       
       END PROGRAM ROUNDED-AVG.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPECIALFILTER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-COUNT PIC S9(10) VALUE 0.
           05 WS-NUM PIC S9(10).
           05 WS-ABS-NUM PIC 9(10).
           05 WS-FIRST-DIGIT PIC 9.
           05 WS-LAST-DIGIT PIC 9.
           05 WS-TEMP-NUM PIC 9(10).
           05 WS-DIGIT-COUNT PIC 9(4).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMS OCCURS 6 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Write a function that takes an array of numbers as input and returns
      * the number of elements in the array that are greater than 10 and both
      * first and last digits of a number are odd (1, 3, 5, 7, 9).
      * For example:
      * specialFilter([15, -73, 14, -15, 8, 9]) => 1
      * specialFilter([33, -2, -3, 45, 21, 109]) => 2
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 6
               MOVE L-NUMS(NI) TO WS-NUM
               IF WS-NUM > 10 THEN
                   COMPUTE WS-ABS-NUM = FUNCTION ABS(WS-NUM)
                   MOVE WS-ABS-NUM TO WS-TEMP-NUM
                   PERFORM GET-FIRST-DIGIT
                   PERFORM GET-LAST-DIGIT
                   IF WS-FIRST-DIGIT = 1 OR 3 OR 5 OR 7 OR 9
                       AND WS-LAST-DIGIT = 1 OR 3 OR 5 OR 7 OR 9
                   THEN
                       ADD 1 TO WS-COUNT
                   END-IF
               END-IF
           END-PERFORM
           MOVE WS-COUNT TO RESULT
           GOBACK.

       GET-FIRST-DIGIT.
           PERFORM UNTIL WS-TEMP-NUM < 10
               DIVIDE WS-TEMP-NUM BY 10 GIVING WS-TEMP-NUM
           END-PERFORM
           MOVE WS-TEMP-NUM TO WS-FIRST-DIGIT.

       GET-LAST-DIGIT.
           COMPUTE WS-LAST-DIGIT = FUNCTION MOD(WS-ABS-NUM 10).

       END PROGRAM SPECIALFILTER.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-NUMS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-TOTAL-SUM   PIC S9(10).
       01 WS-CURRENT-NUM PIC S9(10).
       01 WS-DIGIT       PIC S9(01).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Write a function count_nums which takes an array of integers and returns
      * the number of elements which has a sum of digits > 0.
      * If a number is negative, then its first signed digit will be negative:
      * e.g. -123 has signed digits -1, 2, and 3.
      * >>> count_nums([-1, 11, -11]) == 1
      * >>> count_nums([1, 1, 2]) == 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           SET RESULT TO 0

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               MOVE L-ARR(NI) TO WS-CURRENT-NUM
               SET WS-TOTAL-SUM TO 0

               PERFORM UNTIL WS-CURRENT-NUM = 0
                   IF WS-CURRENT-NUM < 0 THEN
                       MOVE FUNCTION NUMVALC(FUNCTION REPLACE(FUNCTION TRIM(LEADING '-' FROM FUNCTION NUMVALC(WS-CURRENT-NUM)) WITH SPACE BY '')) 
                            TO WS-CURRENT-NUM
                       SUBTRACT FUNCTION MOD(WS-CURRENT-NUM, 10) FROM WS-TOTAL-SUM
                   ELSE
                       MOVE FUNCTION MOD(WS-CURRENT-NUM, 10) TO WS-DIGIT
                       ADD WS-DIGIT TO WS-TOTAL-SUM
                       DIVIDE WS-CURRENT-NUM BY 10 GIVING WS-CURRENT-NUM
                   END-IF
               END-PERFORM

               IF WS-TOTAL-SUM > 0 THEN
                   ADD 1 TO RESULT
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM COUNT-NUMS.

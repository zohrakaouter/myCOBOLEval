       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNT-NUMS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-NUMBER PIC S9(10).
           05 WS-SUM PIC S9(10) VALUE 0.
           05 WS-DIGIT PIC S9(10).
           05 WS-TEMP PIC S9(10).
           05 WS-COUNT PIC S9(10) VALUE 0.
           05 WS-NEGATIVE-FLAG PIC X VALUE 'N'.
               88 IS-NEGATIVE VALUE 'Y'.
               88 IS-POSITIVE VALUE 'N'.

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
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               MOVE L-ARR(NI) TO WS-NUMBER
               MOVE 0 TO WS-SUM
               MOVE 'N' TO WS-NEGATIVE-FLAG

               IF WS-NUMBER < 0
                   SET IS-NEGATIVE TO TRUE
                   MULTIPLY WS-NUMBER BY -1 GIVING WS-TEMP
               ELSE
                   MOVE WS-NUMBER TO WS-TEMP
               END-IF

               PERFORM UNTIL WS-TEMP = 0
                   DIVIDE WS-TEMP BY 10 GIVING WS-TEMP REMAINDER WS-DIGIT
                   IF IS-NEGATIVE AND WS-TEMP = 0
                       MULTIPLY WS-DIGIT BY -1 GIVING WS-DIGIT
                   END-IF
                   ADD WS-DIGIT TO WS-SUM
               END-PERFORM

               IF WS-SUM > 0
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM

           MOVE WS-COUNT TO RESULT
           GOBACK.
       END PROGRAM COUNT-NUMS.

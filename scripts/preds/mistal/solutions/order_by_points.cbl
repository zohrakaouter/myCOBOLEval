       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDER-BY-POINTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-I PIC S9(10) COMP.
           05 WS-J PIC S9(10) COMP.
           05 WS-TEMP PIC S9(10).
           05 WS-SUM-I PIC S9(10) COMP.
           05 WS-SUM-J PIC S9(10) COMP.
           05 WS-NUM PIC S9(10).
           05 WS-DIGIT PIC S9(4).
           05 WS-ABS-NUM PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMS OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * 
      * Write a function which sorts the given list of integers
      * in ascending order according to the sum of their digits.
      * Note: if there are several items with similar sum of their digits,
      * order them based on their index in original list.
      * 
      * For example:
      * >>> order_by_points([1, 11, -1, -11, -12]) == [-1, -11, 1, -12, 11]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE L-NUMS(WS-I) TO RESULT(WS-I)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > 5
                   COMPUTE WS-SUM-I = FUNCTION ABS(RESULT(WS-I))
                   COMPUTE WS-SUM-J = FUNCTION ABS(RESULT(WS-J))

                   PERFORM CALCULATE-DIGIT-SUM WITH TEST AFTER
                       UNTIL WS-ABS-NUM = 0
                       DIVIDE WS-ABS-NUM BY 10 GIVING WS-ABS-NUM
                           REMAINDER WS-DIGIT
                       ADD WS-DIGIT TO WS-SUM-I
                   END-PERFORM

                   MOVE RESULT(WS-J) TO WS-ABS-NUM
                   MOVE 0 TO WS-SUM-J
                   PERFORM CALCULATE-DIGIT-SUM WITH TEST AFTER
                       UNTIL WS-ABS-NUM = 0
                       DIVIDE WS-ABS-NUM BY 10 GIVING WS-ABS-NUM
                           REMAINDER WS-DIGIT
                       ADD WS-DIGIT TO WS-SUM-J
                   END-PERFORM

                   IF WS-SUM-I > WS-SUM-J
                       MOVE RESULT(WS-I) TO WS-TEMP
                       MOVE RESULT(WS-J) TO RESULT(WS-I)
                       MOVE WS-TEMP TO RESULT(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM

           GOBACK.

       CALCULATE-DIGIT-SUM.
           MOVE FUNCTION ABS(RESULT(WS-I)) TO WS-ABS-NUM
           MOVE 0 TO WS-SUM-I.

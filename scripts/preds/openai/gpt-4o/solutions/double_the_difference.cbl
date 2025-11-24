       IDENTIFICATION DIVISION.
       PROGRAM-ID. DOUBLE-THE-DIFFERENCE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ITEMS.
           05 WS-NUMBER PIC S9(10).
           05 WS-SQUARE PIC S9(10).
           05 WS-SUM PIC S9(10) VALUE 0.
           05 I PIC 9(2) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 3 TIMES INDEXED BY NI COMP-2.
           05 RESULT PIC S9(10).

      * '''
      * Given a list of numbers, return the sum of squares of the numbers
      * in the list that are odd. Ignore numbers that are negative or not integers.
      * 
      * double_the_difference([1, 3, 2]) == 1 + 9 + 0 = 10
      * double_the_difference([-1, -2, 0]) == 0
      * 
      * 
      * If the input list is empty, return 0.
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 3
               MOVE L-LST(NI) TO WS-NUMBER
               IF WS-NUMBER > 0
                   IF FUNCTION MOD(WS-NUMBER, 2) NOT = 0
                       COMPUTE WS-SQUARE = WS-NUMBER * WS-NUMBER
                       ADD WS-SQUARE TO WS-SUM
                   END-IF
               END-IF
           END-PERFORM

           MOVE WS-SUM TO RESULT

           GOBACK.
       END PROGRAM DOUBLE-THE-DIFFERENCE.
